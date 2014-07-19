package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.TileRenderer

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation
import in.dogue.profundus.world._
import scala.util.Random
import in.dogue.profundus.entities.damagezones.DamageZone
import in.dogue.profundus.entities.pickups.Pickup
import in.dogue.profundus.world.CreatureSpawn
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.world.PickupSpawn

object EntityManager {

  def create(r:Random) = {
    val rng = new Random(r.nextInt())
    EntityManager(Seq(), Seq(), Seq(), Seq(), Seq(), rng)
  }
}

case class EntityManager private (caps:Seq[Capsule], cr:Seq[Creature], picks:Seq[Pickup[_]], ropes:Seq[Rope], cs:Seq[Casque], r:Random) {
  def update(tc:TerrainCache):(Seq[GlobalSpawn], EntityManager) = {
    val upCaps = caps.map{_.update}
    val (upCasque, gs, ws) = cs.map{_.update}.unzip3
    val (done, notDone) = upCaps.partition{_.isDone}
    val explode = done.map{_.getExplode}.flatten
    val (newRopes, pickups) = ropes.map{_.update(tc)}.unzip
    val newEm = copy(caps=notDone,
                     picks=picks.map{_.update} ++ pickups.flatten,
                     ropes=newRopes.flatten,
                     cs=upCasque)
    (explode ++ gs.flatten, newEm.addSpawns(ws.flatten))
  }

  def hitRopes(pos:Cell) = {
    val newRopes = ropes.map { r =>
      if (r.isKillableAt(pos)) {
        r.kill
      } else {
        r
      }
    }
    copy(ropes=newRopes)
  }

  def hitCreatures(pos:Cell, dmg:Int) = {
    val newCr = cr.map { c =>
      if (c.pos == pos) {
        c.kill
      } else {
        c
      }
    }

    val newCs = cs.map { c =>
      if(c.pos == pos) {
        c.beHit(dmg)
      } else {
        c
      }
    }

    copy(cr=newCr, cs=newCs)
  }

  def addSpawns(spawns:Seq[WorldSpawn]) = {
    spawns.foldLeft(this) { case (man, sp) =>
      sp match {
        case CreatureSpawn(cs) => man.spawnCreatures(cs)
        case PickupSpawn(fs) => man.addDrops(fs)
        case CasqueSpawn(cs) => man.spawnCasques(cs)
      }

    }
  }

  def spawnCreatures(cs:Seq[Creature]) = {
    copy(cr=cr++cs)
  }

  def interact(pl:Player):(Player, EntityManager) = {
    (pl, this)
  }

  def doKill(kz:Seq[DamageZone[_]]):(EntityManager, Seq[Particle[A] forSome {type A}]) = {
    val applied = cr.map { c => DamageZone.process(kz, c, c.damage, c.pos)}
    val (newCr, deadCr) = applied.partition { _.live == Alive}
    val (newCs, deadCs) = cs.partition { _.live == Alive}
    val ps = deadCr.map {_.getDeathParticle} ++ deadCs.map{_.getDeathParticle}
    (copy(cr=newCr, cs=newCs), ps)
  }

  def addDrops(gs:Seq[Pickup[_]]) = {
    copy(picks=picks ++ gs)
  }

  def spawnCasques(gs:Seq[Casque]) = {
    copy(cs=cs ++ gs)
  }

  def isRope(ij:Cell) = {
    ropes.exists{_.ropeContains(ij)}
  }

  def spawnCapsule(ij:Cell) = {
    val c = Capsule.create(ij.x, ij.y)
    copy(caps=caps :+ c)
  }

  def spawnRope(r:Rope) = {
    copy(ropes=ropes :+ r)
  }

  def existsSolid(ij:Cell) = {
    caps.exists{_.pos == ij} || cr.exists {_.pos == ij}
  }

  def collectPickups(p:Player):(Player, EntityManager) = {
    val seed = (p, List[Pickup[_]]())
    val (newPl, newGems) = picks.foldLeft(seed) { case ((pl, list), g) =>
      if (g.getPos == pl.pos) {
        (g.collect(pl), list)
      } else {
        (pl, g :: list)
      }
    }
    (newPl, copy(picks=newGems))
  }

  def updateCreatures(w:TerrainCache, ppos:Cell, pState:LivingState):(EntityManager, Seq[DamageZone[_]]) = {
    val (newCr, attacks) = cr.map { _.update(w, ppos, pState, r) }.unzip
    (copy(cr=newCr), attacks.flatten)
  }

  def doGravity(tr:TerrainCache) = {
    val newCaps = caps.map {_.toMassive.update(tr)}
    val newGems = picks.map {_.toMassive.update(tr)}
    val (onscreen, offscreen) = cr.partition { c => tr.isLoaded(c.pos) }
    val newCr = onscreen.map {_.toMassive.update(tr)} ++ offscreen
    copy(caps = newCaps,
         picks = newGems,
         cr = newCr)
  }

  def getLights = cr.map{_.toLight}

  def draw(tr:TileRenderer):TileRenderer = {
    (tr <++< caps.map {_.draw _}
        <++< picks.map{_.draw _}
        <++< ropes.map{_.draw _}
        <++< cr.map{_.draw _}
        <++< cs.map{_.draw _}
      )
  }
}
