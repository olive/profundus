package in.dogue.profundus.experimental

import in.dogue.profundus.entities.{MineralDrop, KillZone, Player, EntityManager}
import in.dogue.profundus.world.{TerrainCache, TerrainManager, World}
import in.dogue.profundus.particles.{Particle, ParticleManager}
import in.dogue.profundus.deformations.Deformation
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.Profundus


sealed trait NewSpawn
case class NewParticles(s:Seq[Particle[_]]) extends NewSpawn
case class NewKillZones(s:Seq[KillZone[_]]) extends NewSpawn
case class NewDeformations(s:Seq[Deformation[_]]) extends NewSpawn
object GreatWorld {

  /** @tparam T T should not be gettable from GreatWorld, it should be an outside value.
    *           otherwise it should be extracted anew from the GreatWorld instance
    */
  type Update[T] = (GreatWorld, T) => (GreatWorld, Seq[NewSpawn])


  def updateCache : Update[Unit] = standard { case (gw, ()) =>
    val ppos = gw.p.pos
    gw.copy(cache=gw.cache.checkPositions(ppos).update(ppos))
  }

  def updateEs : Update[Unit] = { case (gw, ()) =>
    import Profundus._
    val (updates, kills, particles, newEm)  = gw.em.update(gw.cache)
    val ns  = Seq(particles.ns, updates.ns, kills.ns)
    (gw.copy(em=newEm), ns)
  }

  def updateKzs : Update[Unit] = standard { case (gw, ()) =>
    val newKz = gw.kz.map{_.update}.flatten
    gw.copy(kz=newKz)
  }

  def updateGravity : Update[Unit] = standard { case (gw, ()) =>
    val newEm = gw.em.doGravity(gw.cache)
    gw.copy(em=newEm)
  }

  def updateCreatures : Update[Unit] = { case (gw, ()) =>
    import Profundus._
    val pl = gw.p
    val cache = gw.cache
    val (newEm, kills) = gw.em.updateCreatures(cache, pl.pos, pl.state)
    (gw.copy(em=newEm), Seq(kills.ns))
  }

  def updateDeformations :Update[Unit] = standard { case (gw, ()) =>
    import Profundus._
    val ds = gw.ds
    val cache = gw.cache
    val seed = (cache, Seq[MineralDrop](), 0)
    val (deformed, mins, dmg) = ds.foldLeft(seed){case ((tc, mins, dmg), d) =>
      val (nc, drop, hit) = d.apply(tc)
      (nc, drop ++ mins, dmg+hit)
    }
    //val damaged = gw.p.hitTool(dmg) + ???
    val newEm = gw.em.addDrops(mins)
    val newDs = ds.map{_.update}.flatten
    gw.copy(ds = newDs, cache=deformed, em=newEm)
  }


  def allUpdates(gw:GreatWorld):GreatWorld = {
    (gw #+ updateCache
        #+ updateEs
        #+ updateKzs
        #+ updateGravity
        #+ updateCreatures
        #+ updateDeformations
      )
  }

  def standard[T](func:(GreatWorld, T) => GreatWorld): Update[T] = { case (gw, t) =>
    (func(gw, t), Seq())
  }
}
case class GreatWorld(p:Player, em:EntityManager,  mgr:TerrainManager, pm:ParticleManager, cache:TerrainCache, kz:Seq[KillZone[_]] , ds:Seq[Deformation[_]], updates:Seq[(T, GreatWorld.Update[T]) forSome {type T}]) {
  import GreatWorld._

  def update:GreatWorld = {
    val gw = updates.foldLeft(this) { case (w, (t, up)) =>
      doUpdate(t, up)
    }
    gw.copy(updates=Seq())
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


  def draw(tr:TileRenderer):TileRenderer = {
    tr
  }
}
