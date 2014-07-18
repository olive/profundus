package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.TileRenderer

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation
import in.dogue.profundus.world.{WorldSpawn, FoodSpawn, CreatureSpawn, TerrainCache}
import scala.util.Random
import in.dogue.profundus.entities.damagezones.DamageZone
import in.dogue.profundus.entities.pickups.Pickup

object EntityManager {

  def create(r:Random) = {
    val rng = new Random(r.nextInt())
    EntityManager(Seq(), Seq(), Seq(), Seq(), rng)
  }
}

case class EntityManager private (caps:Seq[Capsule], cr:Seq[Creature], picks:Seq[Pickup[_]], ropes:Seq[Rope], r:Random) {
  def update(tc:TerrainCache):(Seq[Deformation[_]], Seq[DamageZone[_]], Seq[Particle[_]], EntityManager) = {
    val upCaps = caps.map{_.update}
    val (done, notDone) = upCaps.partition{_.isDone}
    val (explosions, particles, kz) = done.map{_.getExplode}.unzip3
    val (newRopes, pickups) = ropes.map{_.update(tc)}.unzip
    val newEm = copy(caps=notDone,
                     picks=picks.map{_.update} ++ pickups.flatten,
                     ropes=newRopes.flatten)
    (explosions, kz, particles.flatten, newEm)
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

  def addSpawns(spawns:Seq[WorldSpawn]) = {
    spawns.foldLeft(this) { case (man, sp) =>
      sp match {
        case CreatureSpawn(cs) =>
          man.spawnCreatures(cs)
        case FoodSpawn(fs) => man.addDrops(fs)
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
    val (newCr, dead) = applied.partition { c => (c.live == Alive)}
    val ps = dead.map {_.getDeathParticle}
    (copy(cr=newCr), ps)
  }

  def addDrops(gs:Seq[Pickup[_]]) = {
    copy(picks=picks ++ gs)
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

  def draw(tr:TileRenderer):TileRenderer = {
    (tr <++< caps.map {_.draw _}
        <++< picks.map{_.draw _}
        <++< ropes.map{_.draw _}
        <++< cr.map{_.draw _}
      )
  }
}
