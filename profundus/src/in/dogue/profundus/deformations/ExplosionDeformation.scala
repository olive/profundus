package in.dogue.profundus.deformations

import in.dogue.profundus.world.{WorldSpawn, TerrainCache}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.pickups.Pickup
import in.dogue.profundus.entities.Mattock

object ExplosionDeformation {
  def create(i:Int, j:Int, tickDamage:Int, radius:Int, speed:Int) = {
    ExplosionDeformation(i, j, tickDamage, radius, speed, 0)
  }
}
case class ExplosionDeformation private (i:Int, j:Int, tickDamage:Int, radius:Int, speed:Int, t:Int) {
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

        val (newTc, drops, _, _) = ttc.hit(ij, tickDamage, Mattock)
        (newTc, mins ++ drops)

      }
      (newTc, mins, 0)

    } else {
      (tc, Seq(), 0)//do nothing
    }
  }

  def toDeformation:Deformation[ExplosionDeformation] = {
    Deformation(_.update, _.isDone, _.deform, this)
  }
}
