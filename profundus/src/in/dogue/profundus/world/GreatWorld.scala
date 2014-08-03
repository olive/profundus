package in.dogue.profundus.world


import in.dogue.profundus.particles.{Emitter, ParticleManager, Particle}
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.{Game, Profundus}
import in.dogue.profundus.entities._
import in.dogue.antiqua.Antiqua
import Antiqua._
import scala.util.Random
import in.dogue.profundus.mode.loadout.Loadout
import in.dogue.profundus.deformations.Deformation
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.entities.damagezones.DamageZone
import in.dogue.profundus.lighting.{LightSource, LightManager}
import in.dogue.profundus.input.Controls
import in.dogue.profundus.audio.{MusicManager, SoundManager}
import in.dogue.profundus.doodads.Doodad
import in.dogue.profundus.entities.pickups.Pickup
import com.deweyvm.gleany.data.Recti

sealed trait GlobalMessage
case class NewParticles(s:Seq[Particle]) extends GlobalMessage
case class NewEmitters(s:Seq[Emitter]) extends GlobalMessage
case class NewDamageZones(s:Seq[DamageZone]) extends GlobalMessage
case class NewDeformations(s:Seq[Deformation]) extends GlobalMessage
case class NewMessageBox(mb:GameBox) extends GlobalMessage
case class NewDoodads(s:Seq[Doodad]) extends GlobalMessage
case class NewClimbables(s:Seq[Climbable]) extends GlobalMessage
case class NewEntities(s:Seq[Entity]) extends GlobalMessage
case class NewPickups(s:Seq[Pickup]) extends GlobalMessage
case class NewTransaction(s:Transaction) extends GlobalMessage
case class DestroyEntity(id:EntityId) extends GlobalMessage
object GreatWorld {

  /** @tparam T T should not be gettable from GreatWorld, it should be an outside value.
    *           otherwise it should be extracted anew from the GreatWorld instance
    */
  case class Update[T](f:(GreatWorld, T) => (GreatWorld, Seq[GlobalMessage]), name:Option[String]) {
    def apply = f.apply _
  }

  def withName[T](s:String)(f:(GreatWorld, T) => (GreatWorld, Seq[GlobalMessage])) = Update(f, s.some)
  def stdName[T](s:String)(f:(GreatWorld, T) => GreatWorld) = {
    val ff = standard(f)
    Update(ff, s.some)
  }
  def stdNoName[T](f:(GreatWorld, T) => GreatWorld) = {
    val ff = standard(f)
    Update(ff, None)
  }
  private def updateClimbRope : Update[Unit] = stdName("climbRope") { case (gw, ()) =>
    val em = gw.em
    val p = gw.p
    val curState = p.fall
    val newState = if (em.isClimbable(p.pos) && p.state == Alive) {
      Floating
    } else {
      curState match {
        case Floating => Falling.create
        case s => s
      }
    }
    val newP = Player.setFallState(p, newState)
    gw.setPlayer(newP)
  }

  private def updateItemUse : Update[Unit] = stdName("itemUse") { case (gw, ()) =>
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
        SoundManager.`throw`.play(pp.pos)
        Rope.create(FlyUp.create(pp.pos)).some
      } else if (!tc.isSolid(pp.pos --> pp.face)){
        Rope.create(DropDown.create(pp.pos --> pp.face)).some
      } else {
        None
      }
      state.map { r =>
        (em.spawnClimbables(r.toClimbable.seq), pp.spendRope)
      }.getOrElse {
        doNothing
      }

    } else {
      (em, pp)
    }
    gw.setEm(newEm).setPlayer(newP)
  }

  private def updateTool : Update[Unit] = stdName("updateTool") { case (gw, ()) =>
    val pp = gw.p
    val em = gw.em
    val newEm = pp.toolPos.foldLeft(em) { case (e, p) =>
      val dmg = Damage(pp.getDamage, DamageType.Player)
      e.hitClimbables(p).hitCreatures(p, dmg)
    }
    gw.setEm(newEm)
  }

  private def collectGems : Update[Unit] = stdName("collectMinerals") { case (gw, ()) =>
    val pp = gw.p
    val em = gw.em
    val (newP, newEm) = em.collectPickups(pp)
    gw.setEm(newEm).setPlayer(newP)
  }


  private def updatePlayer : Update[Unit] = stdName("updatePlayer") { case (gw, ()) =>
    val tm = gw.mgr
    val pp = gw.p
    val cache = gw.cache
    val (newCache, newP, gs) = tm.update(cache, pp)
    gw.setTc(newCache).setPlayer(newP).insertSpawns(gs)
  }


  private def updateCache : Update[Unit] = stdName("updateTerrain") { case (gw, ()) =>
    val ppos = gw.p.pos
    val cache = gw.cache
    val (tc, gs) = cache.update(ppos)

    gw.setTc(tc).insertSpawns(gs)
  }

  private def updateEs : Update[Unit] = withName("entityManager") { case (gw, ()) =>
    val (ns, newEm) = gw.em.update(gw.cache)
    (gw.setEm(newEm), ns)
  }

  private def updateKzs : Update[Unit] = stdName("killzones") { case (gw, ()) =>
    val newKz = gw.kz.map{_.update}.flatten
    gw.setKz(newKz)
  }

  private def updateGravity : Update[Unit] = stdName("gravity") { case (gw, ()) =>
    val newEm = gw.em.doGravity(gw.cache)
    gw.setEm(newEm)
  }

  private def updateCreatures : Update[Unit] = withName("entities") { case (gw, ()) =>
    val pl = gw.p
    val cache = gw.cache
    val (newEm, glob) = gw.em.updateCreatures(cache, pl.getInfo)
    (gw.setEm(newEm), glob)
  }

  private def updateDeformations : Update[Unit] = stdName("deformations") { case (gw, ()) =>
    val ds = gw.ds
    val cache = gw.cache
    val seed = (cache, Seq[GlobalMessage]())
    val (deformed, mins) = ds.foldLeft(seed){case ((tc, mins), d) =>
      val (nc, drop, _/*damage from deformations is void*/) = d.apply(tc)
      (nc, drop ++ mins)
    }
    val newDs = ds.map{_.update}.flatten
    gw.setDs(newDs).setTc(deformed).insertSpawns(mins)
  }

  private def updateParticles : Update[Unit] = stdName("particles") { case (gw, ()) =>
    val pm = gw.pm
    val tc = gw.cache
    val (particles, lights) = pm.update(tc)
    val lm = gw.lm
    gw.setPm(particles).setLm(lm.addLights(lights))
  }

  private def killEntities : Update[Unit] = stdName("kills") { case (gw, ()) =>
    val pp = gw.p
    val em = gw.em
    val kz = gw.kz
    val hurtPl = DamageZone.process(kz, pp, pp.damage, pp.pos)
    val (newEm, ps) = em.doKill(kz)
    gw.setEm(newEm).addPs(ps).setPlayer(hurtPl)
  }

  private def playerSelfQuit : Update[Unit] = stdNoName { case (gw, ()) =>
    val pp = gw.p
    if (Controls.Kill.justPressed) {
      gw.setPlayer(pp.kill(DamageType.Player.some))
    } else {
      gw
    }

  }

  private def updateMusicManager : Update[Unit] = stdNoName { case (gw, ()) =>
    val mm = gw.mm
    val p = gw.p
    val newMm = mm.setPlayer(p.y, p.state)
    gw.setMm(newMm)
  }

  private def addDoodadLights : Update[Unit] = stdName("doodadLights") { case (gw, ()) =>
    val doods = gw.doodads.map{_.update}
    val lights = doods.map{_.getLight}.flatten
    gw.addLights(lights).setDoodads(doods)
  }

  private def filterOffscreen : Update[Unit] = stdName("filterOffscreen") { case (gw, ()) =>
    val pl = gw.p
    val r = Recti(0, pl.y, gw.cols, gw.rows)
    val height = r.height
    val leeway = 10
    val rect = Recti(r.x, r.y - (leeway-1)*height, r.width, r.height*10*leeway)
    val filter = new WorldFilter()

    def f[T]: (Seq[T], (T) => Unloadable[T]) => Seq[T] = filter.filter(rect)
    val doods = f[Doodad](gw.doodads, _.toUnloadable)
    val em = gw.em
    val newEm = em.filter(filter, rect)
    gw.setDoodads(doods).setEm(newEm)
  }

  private def doTransactions : Update[Unit] = stdNoName { case (gw, ()) =>
    val pl = gw.p
    val ts = gw.ts
    val em = gw.em
    import Profundus._
    val ((newP, newEm), ws) = fold2((pl, em), ts) { case (t, (p, e)) =>
      t.apply(p, e)
    }
    gw.setTransactions(Seq()).setPlayer(newP).setEm(newEm).insertSpawns(ws)
  }

  def allUpdates(gw:GreatWorld):GreatWorld = {
    (gw #+ updateClimbRope
        #+ updateItemUse
        #+ updateTool
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
        #+ playerSelfQuit
        #+ updateMusicManager
        #+ addDoodadLights
        #+ filterOffscreen
        #+ doTransactions
      )
  }

  private def standard[T](func:(GreatWorld, T) => GreatWorld)
             :(GreatWorld, T) => (GreatWorld, Seq[GlobalMessage]) = { case (gw, t) =>
    (func(gw, t), Seq())
  }


  def create(worldCols:Int, worldRows:Int, screenCols:Int, screenRows:Int, lo:Loadout, r:Random) = {
    val (cache, spawn, spawnFace, gs1) = TerrainCache.create(worldCols, worldRows, r)
    val (tc, gs2) = cache.update(spawn)
    val loadout = if (Game.hasDrill) {
      lo.copy(tool=Drill)
    } else {
      lo
    }
    val p = Player.create(spawn, spawnFace, loadout)
    val em = EntityManager.create(r)
    val tm = new TerrainManager()
    val pm = ParticleManager.create
    val lm = LightManager.create(screenCols, screenRows)
    val gw = GreatWorld(worldCols, worldRows, screenCols, screenRows, p, em, tm, pm, lm, tc, Seq(), Seq(), Seq(), Seq(), Seq(), new MusicManager(0, Alive, worldRows), None).insertSpawns(gs1 ++ gs2)
    allUpdates(gw)
  }
  var rendered = false
}
case class GreatWorld(cols:Int, rows:Int,
                      screenRows:Int, screenCols:Int,
                      p:Player,
                      em:EntityManager,
                      mgr:TerrainManager,
                      pm:ParticleManager,
                      lm:LightManager,
                      cache:TerrainCache,
                      kz:Seq[DamageZone] ,
                      ds:Seq[Deformation],
                      ts:Seq[Transaction],
                      doodads:Seq[Doodad],
                      updates:Seq[(T, GreatWorld.Update[T]) forSome {type T}],
                      mm:MusicManager,
                      gb:Option[GameBox]) {
  import GreatWorld._

  def setPlayer(pl:Player) = copy(p=pl)
  def setEm(em:EntityManager) = copy(em=em)
  def setLm(l:LightManager) = copy(lm=l)
  def setTm(tm:TerrainManager) = copy(mgr=tm)
  def setPm(pm:ParticleManager) = copy(pm = pm)
  def setTc(tc:TerrainCache) = copy(cache=tc)
  def setKz(kz:Seq[DamageZone]) = copy(kz=kz)
  def setDs(ds:Seq[Deformation]) = copy(ds=ds)
  def addPs(s:Seq[Particle]) = copy(pm=pm++s)
  def setMm(m:MusicManager) = copy(mm=m)
  def addEms(ems:Seq[Emitter]) = copy(pm=pm.addEmitters(ems))
  def setGb(b:GameBox) = copy(gb=b.some)
  def addDoodads(doods:Seq[Doodad]) = copy(doodads = doodads ++ doods)
  def setDoodads(doods:Seq[Doodad]) = copy(doodads = doods)
  def addTransaction(t:Transaction) = copy(ts=ts:+t)
  def setTransactions(t:Seq[Transaction]) = copy(ts=t)
  def resetLm = copy(lm = lm.reset)
  def addLights(ls:Seq[LightSource]) = copy(lm=lm.addLights(ls))
  def update:GreatWorld = {
    if (!rendered) {
      cache.render(0, 24, "test.png")
      rendered = true
    }
    //System.exit(1)
    gb match {
      case Some(mb) => copy(gb = gb.map(_.update).flatten)
      case None =>
        updates.foldLeft(this) { case (w, (t, up)) =>
          w.doUpdate(t, up)
        }
    }
  }

  def +#+[T](t:T, up:GreatWorld.Update[T]) = copy(updates=updates :+ ((t, up)))

  def #+(up:GreatWorld.Update[Unit]) = copy(updates=updates :+ (((), up)))

  private def doUpdateWrap[T](t:T, u:Update[T]) = {
    val (gw, ns) = u.apply(this, t)
    gw.insertSpawns(ns)
  }
  //fixme: code clones
  private def doUpdate[T](t:T, u:Update[T]) = {
    def update() = {
      doUpdateWrap(t, u)
    }

    u.name match {
      case Some(name) => Game.updatePerf.track(name) {
        update()
      }
      case None =>
        update()
    }

  }

  private def insertSpawns(seq:Seq[GlobalMessage]) = {
    seq.foldLeft(this) { case (gw, ns) =>
      gw.insertSpawn(ns)
    }
  }

  private def insertSpawn(ns:GlobalMessage) = {
    ns match {
      case NewParticles(s) => addPs(s)
      case NewDamageZones(s) => copy(kz=kz++s)
      case NewDeformations(s) => copy(ds=ds++s)
      case NewEmitters(s) => addEms(s)
      case NewMessageBox(gb) => setGb(gb)
      case NewDoodads(ds) => addDoodads(ds)
      case NewEntities(cs) => copy(em=em.spawnEntities(cs))
      case NewPickups(fs) => copy(em=em.addDrops(fs))
      case NewTransaction(ts) => addTransaction(ts)
      case DestroyEntity(id) => copy(em=em.killEntity(id))
      case NewClimbables(cs) => copy(em=em.spawnClimbables(cs))
    }
  }

  def cameraY(ppos:Cell) = {
    val res = if (ppos.y < 48) {
      val offset = (ppos.y + 48)/2
      val result = -offset + 32//32
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


    val fs = (c:Cell) => if (Game.lightsOff) {
      Seq()
    } else {
      Seq(getFilter(c))
    }

    tr.withMove(cx, cy) { wp =>
      wp.withFilters(fs(wp.origin))(drawWorld) <+?< gb.map{b => b.draw _}
    }
  }

  private def drawWorld(tr:TileRenderer) = {
    (tr <+< cache.draw(p.pos)
        <+< em.draw
        <+< pm.draw
        <++< doodads.map {_.draw _}
        <+< p.draw
    )
  }
}
