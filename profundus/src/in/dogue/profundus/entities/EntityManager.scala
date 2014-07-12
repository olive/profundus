package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.TileRenderer

import in.dogue.antiqua.Implicits
import Implicits._
import in.dogue.profundus.world.World
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.deformations.Deformation

object EntityManager {
  def create = EntityManager(Seq())
}

case class EntityManager private (caps:Seq[(Int,Int,Capsule)]) {
  def update:(Seq[Deformation[_]], Seq[Particle[_]], EntityManager) = {
    val updated = caps.smap{_.update}
    val (done, notDone) = updated.partition{case (_, _, c) => c.isDone}
    val (explosions, particles) = done.map { case (i, j, c) => c.getExplode(i, j) }.unzip
    (explosions, particles.flatten, copy(caps=notDone))
  }

  def spawnCapsule(ij:(Int,Int)) = {
    val c = (ij.x, ij.y, Capsule.create)
    copy(caps=caps :+ c)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <++< caps.map {case (i, j, c) =>
      c.draw(i, j) _
    }
  }
}
