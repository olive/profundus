package in.dogue.profundus.deformations

import in.dogue.profundus.world.{WorldSpawn, TerrainCache}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.pickups.Pickup
import in.dogue.profundus.entities.{Damage, Mattock}

object ExplosionDeformation {
  def create(ij:Cell, tickDamage:Damage, radius:Int, speed:Int) = {
    ExplosionDeformation(ij, tickDamage, radius, speed, 0)
  }
}
case class ExplosionDeformation private (ij:Cell, tickDamage:Damage, radius:Int, speed:Int, t:Int) {
  final val i = ij.x
  final val j = ij.y
  def update = copy(t=t+1)
  def isDone = t > radius*speed

  def deform(tc:TerrainCache) = {
    if (t % speed == 0) {
      val r = t/speed
      val indices = for (p <- i - radius to i + radius;
                         q <- j - radius to j + radius) yield {
        if (scala.math.hypot(i - p, j - q) < r) {
          (p, q).some
        } else {
          None
        }
      }
      val seed = (tc, Seq[WorldSpawn]())
      val (newTc, mins) = indices.flatten.foldLeft(seed) { case ((ttc, mins), ij) =>

        val (newTc, drops, _) = ttc.hit(ij, tickDamage, Mattock)
        (newTc, mins ++ drops)

      }
      (newTc, mins, 0)

    } else {
      (tc, Seq(), 0)//do nothing
    }
  }

  def toDeformation:Deformation = {
    Deformation[ExplosionDeformation](_.update, _.isDone, _.deform, this)
  }
}
