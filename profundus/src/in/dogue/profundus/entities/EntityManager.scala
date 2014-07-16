package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.TileRenderer

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation
import in.dogue.profundus.world.TerrainCache
import scala.util.Random
import in.dogue.antiqua.data.Direction

object EntityManager {

  def create(r:Random) = {
    val rng = new Random(r.nextInt())
    EntityManager(Seq(), Seq(), Seq(), Seq(), rng)
  }
}

case class EntityManager private (caps:Seq[Capsule], cr:Seq[Creature], gems:Seq[MineralDrop], ropes:Seq[Rope], r:Random) {
  def update(tc:TerrainCache):(Seq[Deformation[_]], Seq[KillZone[_]], Seq[Particle[_]], EntityManager) = {
    val upCaps = caps.map{_.update}
    val (done, notDone) = upCaps.partition{_.isDone}
    val (explosions, particles, kz) = done.map{_.getExplode}.unzip3
    val newEm = copy(caps=notDone,
                     gems=gems.map{_.update},
                     ropes=ropes.map{_.update(tc)})
    (explosions, kz, particles.flatten, newEm)
  }

  def spawnCreatures(cs:Seq[Creature]) = {
    copy(cr=cr++cs)
  }

  def interact(pl:Player):(Player, EntityManager) = {
    (pl, this)
  }

  def doKill(kz:Seq[KillZone[_]]):(EntityManager, Seq[Particle[A] forSome {type A}]) = {
    val (newCr, dead) = cr.partition { c => !kz.exists { _.contains(c.pos)} }
    val ps = dead.map {_.getDeathParticle}
    (copy(cr=newCr), ps)
  }

  def addDrops(gs:Seq[MineralDrop]) = {
    copy(gems=gems ++ gs)
  }

  def isRope(ij:(Int,Int)) = {
    ropes.exists{_.ropeContains(ij)}
  }

  def spawnCapsule(ij:(Int,Int)) = {
    val c = Capsule.create(ij.x, ij.y)
    copy(caps=caps :+ c)
  }

  def spawnRope(ij:(Int,Int), d:Direction) = {
    val r = Rope.create(ij, d)
    copy(ropes=ropes :+ r)
  }

  def existsSolid(ij:(Int,Int)) = {
    caps.exists{_.pos == ij} || cr.exists {_.pos == ij}
  }

  def collectGems(p:Player):(Player, EntityManager) = {
    val (newPl, newGems) = gems.foldLeft((p, List[MineralDrop]())) { case ((pl, list), g) =>
      if (g.pos == p.pos) {
        (p.collect(g), list)
      } else {
        (p, g :: list)
      }
    }
    (newPl, copy(gems=newGems))
  }

  def updateCreatures(w:TerrainCache, ppos:(Int,Int), pState:LivingState):(EntityManager, Seq[KillZone[_]]) = {
    val (newCr, attacks) = cr.map { _.update(w, ppos, pState, r) }.unzip
    (copy(cr=newCr), attacks.flatten)
  }

  def doGravity(tr:TerrainCache) = {
    val newCaps = caps.map {_.toMassive.update(tr)}
    val newGems = gems.map {_.toMassive.update(tr)}
    val (offscreen, onscreen) = cr.partition { c => tr.isLoaded(c.pos) }
    val newCr = onscreen.map {_.toMassive.update(tr)} ++ offscreen
    copy(caps = newCaps,
         gems = newGems,
         cr = newCr)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    (tr <++< caps.map {_.draw _}
        <++< gems.map{_.draw _}
        <++< ropes.map{_.draw _}
        <++< cr.map{_.draw _}
      )
  }
}
