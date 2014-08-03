package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.TileRenderer

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.world._
import scala.util.Random
import in.dogue.profundus.entities.damagezones.DamageZone
import in.dogue.profundus.entities.pickups.Pickup
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.audio.SoundManager
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.Game

object EntityManager {

  def create(r:Random) = {
    val rng = new Random(r.nextInt())
    EntityManager(Seq(), Seq(), Seq(), Seq(), rng)
  }
}

case class EntityManager private (caps:Seq[Capsule], cr:Seq[Entity], picks:Seq[Pickup], climbs:Seq[Climbable], r:Random) {
  def update(tc:TerrainCache):(Seq[GlobalMessage], EntityManager) = {
    val upCaps = caps.map{_.update}
    val (done, notDone) = upCaps.partition{_.isDone}
    done.headOption.foreach { d =>
      SoundManager.boom.play(d.pos)
    }
    val explode = done.map{_.getExplode}.flatten
    val upped: Seq[(Option[Climbable], Seq[GlobalMessage])] = climbs.map{_.update(tc)}
    val (newClimbs, pickups) = upped.unzip
    val newEm = copy(caps=notDone,
                     picks=picks.map{_.update},
                     climbs=newClimbs.flatten)
    (explode ++ pickups.flatten, newEm)
  }

  def filter(wf:WorldFilter, recti:Recti) = {
    def f[T]: (Seq[T], (T) => Unloadable[T]) => Seq[T] = wf.filter(recti)
    val newCaps = f[Capsule](caps, _.toUnloadable)
    val newClimb = f[Climbable](climbs, _.toUnloadable)
    val newCreatures = f[Entity](cr, _.toUnloadable)
    val newPickups = f[Pickup](picks, _.toUnloadable)
    copy(caps=newCaps, cr=newCreatures, picks=newPickups, climbs=newClimb)
  }

  def hitClimbables(pos:Cell) = {
    val newClimb = climbs.map { r =>
      if (r.isKillableAt(pos)) {
        r.kill
      } else {
        r
      }
    }
    copy(climbs=newClimb)
  }

  def hitCreatures(pos:Cell, dmg:Damage) = {
    val newCr = cr.map { c =>
      if (c.pos == pos) {
        c.damage(dmg)
      } else {
        c
      }
    }

    copy(cr=newCr)
  }


  def spawnEntities(cs:Seq[Entity]) = {
    copy(cr=cr++cs)
  }

  def interact(pl:Player):(Player, EntityManager) = {
    (pl, this)
  }

  def doKill(kz:Seq[DamageZone]):(EntityManager, Seq[Particle]) = {
    val applied = cr.map { c => DamageZone.process(kz, c, c.damage, c.pos)}
    val (newCr, deadCr) = applied.partition { _.getLiving == Alive}
    val ps = deadCr.map {_.getDeathParticle}
    (copy(cr=newCr), ps)
  }

  def killEntity(id:EntityId) = {
    val alive = cr.filter{e => ! e.isId(id)}
    copy(cr = alive)
  }

  def addDrops(gs:Seq[Pickup]) = {
    copy(picks=picks ++ gs)
  }

  def isClimbable(ij:Cell) = {
    climbs.exists{_.contains(ij)}
  }

  def spawnCapsule(ij:Cell) = {
    SoundManager.shhh.play(ij)
    val c = Capsule.create(ij)
    copy(caps=caps :+ c)
  }

  def spawnClimbables(rs:Seq[Climbable]) = {
    copy(climbs=climbs ++ rs)
  }

  def existsSolid(ij:Cell) = {
    caps.exists{_.pos == ij} || cr.exists {_.pos == ij}
  }

  def collectPickups(p:Player):(Player, EntityManager) = {
    val seed = (p, List[Pickup]())
    val (newPl, newPicks) = picks.foldLeft(seed) { case ((pl, list), pick) =>
      if (pick.isCollectable(pl) && pick.getPos == pl.pos) {
        (pick.collect(pl), list)
      } else {
        (pl, pick :: list)
      }
    }
    (newPl, copy(picks=newPicks))
  }

  def removePickup(item:Pickup) = {
    val newPicks = picks.filter{i => !i.equals(item)}
    copy(picks = newPicks)
  }

  def updateCreatures(w:TerrainCache, pi:PlayerInfo):(EntityManager, Seq[GlobalMessage]) = {
    val (newCr, glob) = cr.map { c =>
      val thing: ((Entity, Seq[GlobalMessage])) = c.update(w, pi, r)
      thing
    }.unzip
    (copy(cr=newCr), glob.flatten)
  }

  def doGravity(tr:TerrainCache) = {
    val newCaps = caps.map {_.toMassive.update(tr)}
    val newGems = picks.map {_.toMassive.update(tr)}
    val (onscreen, offscreen) = cr.partition { c => tr.isLoaded(c.pos) }
    val newCr:Seq[Entity] = onscreen.map { (c:Entity) =>

      val thing = c.toMassive
      val r = thing.update(tr)
      r
    } ++ offscreen
    copy(caps = newCaps,
         picks = newGems,
         cr = newCr)
  }

  def getLights = cr.map{_.getLight}.flatten

  def draw(tr:TileRenderer):TileRenderer = {
    (tr <++< caps.map {_.draw _}
        <++< picks.map{_.draw _}
        <++< climbs.map{_.draw _}
        <++< cr.map{_.draw _}
      )
  }
}
