package in.dogue.profundus.world


import in.dogue.profundus.particles.{Emitter, ParticleManager, Particle}
import in.dogue.antiqua.graphics.{Filter, Tile, TileRenderer}
import in.dogue.profundus.{Game, Profundus}
import in.dogue.profundus.entities._
import in.dogue.antiqua.Antiqua
import Antiqua._
import scala.util.Random
import in.dogue.profundus.mode.loadout.Loadout
import in.dogue.profundus.deformations.Deformation
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.entities.damagezones.DamageZone
import in.dogue.profundus.entities.pickups.Pickup
import in.dogue.profundus.lighting.{LightSource, LightManager}
import in.dogue.profundus.input.Controls
import in.dogue.profundus.audio.SoundManager

sealed trait GlobalSpawn
case class NewParticles(s:Seq[Particle[_]]) extends GlobalSpawn
case class NewEmitters(s:Seq[Emitter[_]]) extends GlobalSpawn
case class NewDamageZones(s:Seq[DamageZone[_]]) extends GlobalSpawn
case class NewDeformations(s:Seq[Deformation[_]]) extends GlobalSpawn
object GreatWorld {

  /** @tparam T T should not be gettable from GreatWorld, it should be an outside value.
    *           otherwise it should be extracted anew from the GreatWorld instance
    */
  type Update[T] = (GreatWorld, T) => (GreatWorld, Seq[GlobalSpawn])


  private def updateClimbRope : Update[Unit] = standard { case (gw, ()) =>
    val em = gw.em
    val p = gw.p
    val curState = p.fall
    val newState = if (em.isRope(p.pos) && p.state == Alive) {
      Floating
    } else {
      curState match {
        case Floating => Falling.create
        case s => s
      }
    }
    val newP = p.setFallState(newState)
    gw.setPlayer(newP)
  }

  private def updateItemUse : Update[Unit] = standard { case (gw, ()) =>
    val pp = gw.p
    val em = gw.em
    val tc = gw.cache
    val doNothing = (em, pp)
    val (newEm, newP) = if (pp.ctrl.isBombing && pp.inv.hasBomb) {
      val capPos = pp.pos --> pp.face
      if (!tc.isSolid(capPos)) {
        (em.spawnCapsule(capPos), pp.spendBomb)
      } else {
        doNothing
      }
    } else if (pp.ctrl.isRoping && pp.inv.hasRope) {
      val state = if (pp.face.isVertical && !tc.isSolid(pp.pos --> Direction.Up)) {
        SoundManager.`throw`.play()
        Rope.create(FlyUp.create(pp.pos)).some
      } else if (!tc.isSolid(pp.pos --> pp.face)){
        Rope.create(DropDown.create(pp.pos --> pp.face)).some
      } else {
        None
      }
      state.map { r =>
        (em.spawnRope(r), pp.spendRope)
      }.getOrElse {
        doNothing
      }

    } else {
      (em, pp)
    }
    gw.setEm(newEm).setPlayer(newP)
  }

  private def updateShovel : Update[Unit] = standard { case (gw, ()) =>
    val pp = gw.p
    val em = gw.em
    val newEm = pp.toolPos match {
      case None => em
      case Some(pos) =>
        val dmg = Damage(pp.inv.tool.`type`.digDamage, DamageType.Player)
        em.hitRopes(pos).hitCreatures(pos, dmg/*fixme*/)

    }
    gw.setEm(newEm)
  }

  private def collectGems : Update[Unit] = standard { case (gw, ()) =>
    val pp = gw.p
    val em = gw.em
    val (newP, newEm) = em.collectPickups(pp)
    gw.setEm(newEm).setPlayer(newP)
  }


  private def updatePlayer : Update[Unit] = standard { case (gw, ()) =>
    val tm = gw.mgr
    val pp = gw.p
    val cache = gw.cache
    val (newCache, newP, gs, ws) = tm.update(cache, pp)
    val newEm = gw.em.addSpawns(ws)
    gw.setTc(newCache).setPlayer(newP).setEm(newEm).insertSpawns(gs)
  }


  private def updateCache : Update[Unit] = standard { case (gw, ()) =>
    val ppos = gw.p.pos
    val cache = gw.cache
    val em = gw.em
    val (tc, spawns, gs) = cache.checkPositions(ppos)
    val newEm = em.addSpawns(spawns)
    val (newTc, newLights) = tc.update(ppos)
    val lm = gw.lm

    gw.setTc(newTc).setEm(newEm).setLm(lm.addLights(newLights)).insertSpawns(gs)
  }

  private def updateEs : Update[Unit] = { case (gw, ()) =>
    val (ns, newEm) = gw.em.update(gw.cache)
    (gw.setEm(newEm), ns)
  }

  private def updateKzs : Update[Unit] = standard { case (gw, ()) =>
    val newKz = gw.kz.map{_.update}.flatten
    gw.setKz(newKz)
  }

  private def updateGravity : Update[Unit] = standard { case (gw, ()) =>
    val newEm = gw.em.doGravity(gw.cache)
    gw.setEm(newEm)
  }

  private def updateCreatures : Update[Unit] = { case (gw, ()) =>
    val pl = gw.p
    val cache = gw.cache
    val (newEm, glob, worl) = gw.em.updateCreatures(cache, pl.pos, pl.state)
    val insertedEm = newEm.addSpawns(worl)
    (gw.setEm(insertedEm), glob)
  }

  private def updateDeformations : Update[Unit] = standard { case (gw, ()) =>
    val ds = gw.ds
    val cache = gw.cache
    val seed = (cache, Seq[WorldSpawn]())
    val (deformed, mins) = ds.foldLeft(seed){case ((tc, mins), d) =>
      val (nc, drop, _) = d.apply(tc)
      (nc, drop ++ mins)
    }
    val newEm = gw.em.addSpawns(mins)
    val newDs = ds.map{_.update}.flatten
    gw.setDs(newDs).setTc(deformed).setEm(newEm)
  }

  private def updateParticles : Update[Unit] =  standard { case (gw, ()) =>
    val pm = gw.pm
    val tc = gw.cache
    val (particles, lights) = pm.update(tc)
    val lm = gw.lm
    gw.setPm(particles).setLm(lm.addLights(lights))
  }

  private def killEntities : Update[Unit] = standard { case (gw, ()) =>
    val pp = gw.p
    val em = gw.em
    val kz = gw.kz
    val hurtPl = DamageZone.process(kz, pp, pp.damage, pp.pos)
    val (newEm, ps) = em.doKill(kz)
    gw.setEm(newEm).addPs(ps).setPlayer(hurtPl)
  }

  private def playerKillSelf : Update[Unit] = standard { case (gw, ()) =>
    val pp = gw.p
    if (Controls.Kill.justPressed) {
      gw.setPlayer(pp.kill(DamageType.Player.some))
    } else {
      gw
    }

  }


  def allUpdates(gw:GreatWorld):GreatWorld = {
    (gw #+ updateClimbRope
        #+ updateItemUse
        #+ updateShovel
        #+ collectGems
        #+ updatePlayer
        #+ updateCache
        #+ updateEs
        #+ updateKzs
        #+ updateGravity
        #+ updateCreatures
        #+ updateDeformations
        #+ updateParticles
        #+ killEntities
        #+ playerKillSelf
      )
  }

  private def standard[T](func:(GreatWorld, T) => GreatWorld) : Update[T] = { case (gw, t) =>
    (func(gw, t), Seq())
  }


  def create(cols:Int, rows:Int, lo:Loadout, r:Random) = {
    val (cache, spawn, spawnFace) = TerrainCache.create(cols, rows, r)
    val (tc, cs, gs) = cache.checkPositions(spawn)
    val p = Player.create(spawn, spawnFace, lo)
    val em = EntityManager.create(r).addSpawns(cs)
    val tm = new TerrainManager()
    val pm = ParticleManager.create
    val lm = LightManager.create
    val gw = GreatWorld(p, em, tm, pm, lm, tc, Seq(), Seq(), Seq()).insertSpawns(gs)
    allUpdates(gw)
  }
}
case class GreatWorld(p:Player, em:EntityManager,  mgr:TerrainManager, pm:ParticleManager, lm:LightManager, cache:TerrainCache, kz:Seq[DamageZone[_]] , ds:Seq[Deformation[_]], updates:Seq[(T, GreatWorld.Update[T]) forSome {type T}]) {
  import GreatWorld._

  def setPlayer(pl:Player) = copy(p=pl)
  def setEm(em:EntityManager) = copy(em=em)
  def setLm(l:LightManager) = copy(lm=l)
  def setTm(tm:TerrainManager) = copy(mgr=tm)
  def setPm(pm:ParticleManager) = copy(pm = pm)
  def setTc(tc:TerrainCache) = copy(cache=tc)
  def setKz(kz:Seq[DamageZone[_]]) = copy(kz=kz)
  def setDs(ds:Seq[Deformation[_]]) = copy(ds=ds)
  def addPs(s:Seq[Particle[_]]) = copy(pm=pm++s)
  def addEms(ems:Seq[Emitter[_]]) = copy(pm=pm.addEmitters(ems))

  def update:GreatWorld = {
    updates.foldLeft(this) { case (w, (t, up)) =>
      w.doUpdate(t, up)
    }
  }

  def +#+[T](t:T, up:GreatWorld.Update[T]) = copy(updates=updates :+ ((t, up)))

  def #+(up:GreatWorld.Update[Unit]) = copy(updates=updates :+ (((), up)))

  private def doUpdate[T](t:T, u:Update[T]) = {
    val (gw, ns) = u(this, t)
    gw.insertSpawns(ns)

  }

  private def insertSpawns(seq:Seq[GlobalSpawn]) = {
    seq.foldLeft(this) { case (gw, ns) =>
      gw.insertSpawn(ns)
    }
  }

  private def insertSpawn(ns:GlobalSpawn) = {
    ns match {
      case NewParticles(s) => addPs(s)
      case NewDamageZones(s) => copy(kz=kz++s)
      case NewDeformations(s) => copy(ds=ds++s)
      case NewEmitters(s) => addEms(s)
    }
  }

  def cameraY(ppos:Cell) = {
    val res = if (ppos.y < 48) {
      val offset = (ppos.y + 48)/2
      val result = -offset + 32
      result
    } else {
      -15
    }
    math.min(res, 0)
  }

  def assembleLights:Seq[LightSource] = {
    Seq(p.toLight) ++ em.getLights
  }

  def getFilter(cxy:Cell) = {
    val newLm = assembleLights.foldLeft(lm) { case (l, ls) =>
      l.addLight(ls)
    }
    newLm.getFilter(cxy)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    val screenSize = 32
    val cols = 32*4
    val cameraX = p.x.clamp(screenSize/2, cols - screenSize/2)
    val cx = -cameraX + 16
    val cy = -p.y + cameraY(p.pos)


    val res = tr.withMove(cx, cy) { wp =>
      if (!Game.lightsOff) {
        wp.withFilter(getFilter(wp.origin)) { wp =>
          wp <+< drawWorld
        }
      } else {
        wp <+< drawWorld
      }

    }

    res
  }

  private def drawWorld(tr:TileRenderer) = {
    (tr <+< cache.draw(p.pos)
      <+< em.draw
      <+< p.draw
      <+< pm.draw
      )
  }
}
