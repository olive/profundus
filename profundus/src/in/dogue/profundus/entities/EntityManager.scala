package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.TileRenderer

import in.dogue.antiqua.Implicits
import Implicits._
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation
import in.dogue.profundus.world.World

object EntityManager {
  def create = EntityManager(Seq(), Seq())
}

case class EntityManager private (caps:Seq[Capsule], gems:Seq[MineralDrop]) {
  def update:(Seq[Deformation[_]], Seq[Particle[_]], EntityManager) = {
    val updated = caps.map{_.update}
    val (done, notDone) = updated.partition{ _.isDone}
    val (explosions, particles) = done.map { _.getExplode }.unzip
    (explosions, particles.flatten, copy(caps=notDone, gems=gems.map{_.update}))
  }

  def addDrops(gs:Seq[MineralDrop]) = {
    copy(gems=gems ++ gs)
  }

  def spawnCapsule(ij:(Int,Int)) = {
    val c = Capsule.create(ij.x, ij.y)
    copy(caps=caps :+ c)
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
    tr <++< caps.map {_.draw _ } <++< gems.map{_.draw _}
  }
}
