package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.TileRenderer

import in.dogue.antiqua.Implicits
import Implicits._
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation
import in.dogue.profundus.world.World

object EntityManager {
  def create = EntityManager(Seq())
}

case class EntityManager private (caps:Seq[Capsule]) {
  def update:(Seq[Deformation[_]], Seq[Particle[_]], EntityManager) = {
    val updated = caps.map{_.update}
    val (done, notDone) = updated.partition{ _.isDone}
    val (explosions, particles) = done.map { _.getExplode }.unzip
    (explosions, particles.flatten, copy(caps=notDone))
  }

  def spawnCapsule(ij:(Int,Int)) = {
    val c = Capsule.create(ij.x, ij.y)
    copy(caps=caps :+ c)
  }

  def doGravity(w:World) = {
    copy(caps = caps.map { _.toMassive.update(w) })
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< caps.map {_.draw _ }
  }
}
