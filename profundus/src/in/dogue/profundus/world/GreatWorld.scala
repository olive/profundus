package in.dogue.profundus.world


import in.dogue.profundus.particles.{ParticleManager}
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.Profundus
import in.dogue.profundus.entities._
import in.dogue.antiqua.Antiqua
import Antiqua._
import scala.util.Random
import in.dogue.profundus.mode.loadout.Loadout
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation
import in.dogue.profundus.entities.KillZone
import in.dogue.antiqua.data.Direction

sealed trait NewSpawn
case class NewParticles(s:Seq[Particle[_]]) extends NewSpawn
case class NewKillZones(s:Seq[KillZone[_]]) extends NewSpawn
case class NewDeformations(s:Seq[Deformation[_]]) extends NewSpawn
object GreatWorld {

  /** @tparam T T should not be gettable from GreatWorld, it should be an outside value.
    *           otherwise it should be extracted anew from the GreatWorld instance
    */
  type Update[T] = (GreatWorld, T) => (GreatWorld, Seq[NewSpawn])


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
    val (newEm, newP) = if (pp.isBombing && pp.inv.hasBomb) {
      val capPos = pp.pos --> pp.face
      if (!tc.isSolid(capPos)) {
        (em.spawnCapsule(capPos), pp.spendBomb)
      } else {
        doNothing
      }
    } else if (pp.isRoping && pp.inv.hasRope) {
      val state = if (pp.face.isVertical && !tc.isSolid(pp.pos --> Direction.Up)) {
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
    val newEm = pp.shovelPos match {
      case None => em
      case Some(pos) => em.hitRopes(pos)

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
    val (newCache, newP, drops, ps) = tm.update(cache, pp)
    val newEm = gw.em.addDrops(drops)
    gw.setTc(newCache).setPlayer(newP).setEm(newEm).addPs(ps)
  }


  private def updateCache : Update[Unit] = standard { case (gw, ()) =>
    val ppos = gw.p.pos
    val cache = gw.cache
    val (tc, cs) = cache.checkPositions(ppos)
    val em = gw.em.spawnCreatures(cs)
    gw.setTc(tc.update(ppos)).setEm(em)
  }

  private def updateEs : Update[Unit] = { case (gw, ()) =>
    import Profundus._
    val (updates, kills, particles, newEm) = gw.em.update(gw.cache)
    val ns  = Seq(particles.ns, updates.ns, kills.ns)
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
    import Profundus._
    val pl = gw.p
    val cache = gw.cache
    val (newEm, kills) = gw.em.updateCreatures(cache, pl.pos, pl.state)
    (gw.setEm(newEm), Seq(kills.ns))
  }

  private def updateDeformations :Update[Unit] = standard { case (gw, ()) =>
    val ds = gw.ds
    val cache = gw.cache
    val seed = (cache, Seq[Pickup[_]]())
    val (deformed, mins) = ds.foldLeft(seed){case ((tc, mins), d) =>
      val (nc, drop, _) = d.apply(tc)
      (nc, drop ++ mins)
    }
    val newEm = gw.em.addDrops(mins)
    val newDs = ds.map{_.update}.flatten
    gw.setDs(newDs).setTc(deformed).setEm(newEm)
  }

  private def updateParticles : Update[Unit] =  standard { case (gw, ()) =>
    gw.setPm(gw.pm.update)
  }

  private def killEntities : Update[Unit] = standard { case (gw, ()) =>
    val pp = gw.p
    val em = gw.em
    val kz = gw.kz
    val pl = if (kz.exists{ _.contains(pp.pos)}) {
      pp.kill
    } else{
      pp
    }
    val (newEm, ps) = em.doKill(kz)
    gw.setEm(newEm).addPs(ps).setPlayer(pl)
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
      )
  }

  private def standard[T](func:(GreatWorld, T) => GreatWorld): Update[T] = { case (gw, t) =>
    (func(gw, t), Seq())
  }


  def create(cols:Int, rows:Int, lo:Loadout, r:Random) = {
    val (cache, spawn, spawnFace) = TerrainCache.create(cols, rows, r)
    val (tc, cs) = cache.checkPositions(spawn)
    val p = Player.create(spawn, spawnFace, lo)
    val em = EntityManager.create(r).spawnCreatures(cs)
    val tm = new TerrainManager()
    val pm = ParticleManager.create
    val gw = GreatWorld(p, em, tm, pm, tc, Seq(), Seq(), Seq())
    allUpdates(gw)
  }
}
case class GreatWorld(p:Player, em:EntityManager,  mgr:TerrainManager, pm:ParticleManager, cache:TerrainCache, kz:Seq[KillZone[_]] , ds:Seq[Deformation[_]], updates:Seq[(T, GreatWorld.Update[T]) forSome {type T}]) {
  import GreatWorld._

  def setPlayer(p:Player) = copy(p=p)
  def setEm(em:EntityManager) = copy(em=em)
  def setTm(tm:TerrainManager) = copy(mgr=tm)
  def setPm(pm:ParticleManager) = copy(pm = pm)
  def setTc(tc:TerrainCache) = copy(cache=tc)
  def setKz(kz:Seq[KillZone[_]]) = copy(kz=kz)
  def setDs(ds:Seq[Deformation[_]]) = copy(ds=ds)
  def addPs(s:Seq[Particle[_]]) = copy(pm=pm++s)

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

  private def insertSpawns(seq:Seq[NewSpawn]) = {
    seq.foldLeft(this) { case (gw, ns) =>
      gw.insertSpawn(ns)
    }
  }

  private def insertSpawn(ns:NewSpawn) = {
    ns match {
      case NewParticles(s) => copy(pm=pm++s)
      case NewKillZones(s) => copy(kz=kz++s)
      case NewDeformations(s) => copy(ds=ds++s)
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


  def draw(tr:TileRenderer):TileRenderer = {
    val offset = 0//5
    val screenSize = 32
    val cols = 32*4
    val cameraX = p.x.clamp(screenSize/2, cols - screenSize/2)
    tr.withMove(-cameraX + 16, -p.y - offset + cameraY(p.pos)) { worldPos =>
      (worldPos <+< cache.draw(p.pos)
                <+< em.draw
                <+< p.draw
                <+< pm.draw
      )
    }
  }
}
