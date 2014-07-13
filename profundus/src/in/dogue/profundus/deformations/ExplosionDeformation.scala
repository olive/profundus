package in.dogue.profundus.deformations

import in.dogue.profundus.world.World
import in.dogue.antiqua.Antiqua
import Antiqua._
object ExplosionDeformation {
  def create(i:Int, j:Int, radius:Int, speed:Int) = {
    ExplosionDeformation(i, j, radius, speed, 0)
  }
}
case class ExplosionDeformation private (i:Int, j:Int, radius:Int, speed:Int, t:Int) {
  def update = copy(t=t+1)
  def isDone = t > radius*speed

  def deform(w:World):World = {
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

      indices.flatten.foldLeft(w) { case (wd, pr) =>
        wd.hit(pr)
      }

    } else {
      w//do nothing
    }
  }

  def toDeformation:Deformation[ExplosionDeformation] = {
    Deformation(_.update, _.isDone, _.deform, this)
  }
}
