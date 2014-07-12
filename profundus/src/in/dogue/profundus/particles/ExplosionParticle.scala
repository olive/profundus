package in.dogue.profundus.particles

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Implicits
import Implicits._
import scala.util.Random

object ExplosionParticle {
  def create(i:Int, j:Int, intensity:Int, radius:Int, speed:Int) = {
    ExplosionParticle(i, j, intensity, radius, speed, 0, new Random())
  }
  private final val colors = Vector(Color.Yellow, Color.Orange, Color.Red)
}
case class ExplosionParticle private (i:Int, j:Int, intensity:Int, radius:Int, speed:Int, t:Int, r:Random) {
  import ExplosionParticle._
  def update = copy(t=t+1)
  def isDone = t > (radius+1)*speed
  def toParticle:Particle[ExplosionParticle] = Particle(_.update, _.draw, _.isDone, this)

  def draw(tr:TileRenderer):TileRenderer = {

    val outer = t/speed
    val inner = scala.math.abs((t - speed*4)/speed.toFloat)
    val indices = for (p <- (i - outer) to (i + outer);
                       q <- (j - outer) to (j + outer)) yield {
      val h = scala.math.hypot(i - p, j - q)
      if (h < outer && h > inner) {
        (p, q).some
      } else {
        None
      }
    }
    tr `$$>` (indices.flatten map { case (p, q) =>
      def f(t:Tile):Tile = {
        val bg = colors.randomR(r).dim(0.2 + r.nextDouble)
        val fg = colors.randomR(r).dim(0.2 + r.nextDouble)
        t.setFg(fg).setBg(bg)
      }
      (p, q, f _)
    })
  }
}
