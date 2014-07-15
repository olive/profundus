package in.dogue.profundus.particles

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._

object DeathParticle {
  def create(i:Int, j:Int, maxT:Int) = DeathParticle(i, j, 0, maxT)
}

case class DeathParticle private (i:Int, j:Int, t:Int, maxT:Int){
  final val expandTime = 6
  def isDone = t >= maxT
  def update = copy(t=t+1)
  def draw(tr:TileRenderer):TileRenderer = {
    val b = 4
    val r = math.min((t/expandTime.toDouble)*b, b)
    val prop = (maxT - t)/expandTime.toFloat
    val draws = for (p <- (i - b) to (i + b);
                     q <- (j - b) to (j + b)) yield {
      val h = math.hypot(i - p, j - q)
      val mixAmt = (r - h)/r.toDouble
      val mix = if (t + expandTime >= maxT) {
        mixAmt * prop
      } else {
        mixAmt
      }

      def f(tile:Tile):Tile = tile.mapBg(_.mix(Color.Red, mix/4))
      val func = if (h < r) {
        f _
      } else {
        id[Tile] _
      }
      (p, q, func)
    }
    tr `$$>` draws
  }

  def toParticle:Particle[DeathParticle] = Particle(_.update, _.draw, _.isDone, this)
}
