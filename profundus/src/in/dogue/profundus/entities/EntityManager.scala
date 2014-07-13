package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.TileRenderer

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation
import in.dogue.profundus.world.World

object EntityManager {
  def create = EntityManager(Seq(), Seq(), Seq())
}

case class EntityManager private (caps:Seq[Capsule], gems:Seq[MineralDrop], ropes:Seq[Rope]) {
  def update(w:World):(Seq[Deformation[_]], Seq[KillZone[_]], Seq[Particle[_]], EntityManager) = {
    val upCaps = caps.map{_.update}
    val (done, notDone) = upCaps.partition{_.isDone}
    val (explosions, particles, kz) = done.map{_.getExplode}.unzip3
    val newEm = copy(caps=notDone,
                     gems=gems.map{_.update},
                     ropes=ropes.map{_.update(w)})
    (explosions, kz, particles.flatten, newEm)
  }

  def addDrops(gs:Seq[MineralDrop]) = {
    copy(gems=gems ++ gs)
  }

  def spawnCapsule(ij:(Int,Int)) = {
    val c = Capsule.create(ij.x, ij.y)
    copy(caps=caps :+ c)
  }

  def spawnRope(ij:(Int,Int)) = {
    val r = Rope.create(ij)
    copy(ropes=ropes :+ r)
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

  def doGravity(w:World) = {
    val newCaps = caps.map { _.toMassive.update(w) }
    val newGems = gems.map { _.toMassive.update(w) }
    copy(caps = newCaps,
         gems = newGems)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< caps.map {_.draw _ } <++< gems.map{_.draw _} <++< ropes.map{_.draw _}
  }
}
