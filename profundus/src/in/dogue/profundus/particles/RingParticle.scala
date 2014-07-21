package in.dogue.profundus.particles

import in.dogue.profundus.lighting.LightSource
import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.Antiqua.Cell
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import scala.util.Random
import in.dogue.profundus.world.TerrainCache

object RingParticle {
  def create(ij:Cell, radius:Int, speed:Int) = {
    RingParticle(ij, radius, speed, 0, new Random())
  }
  val colors = Vector(Color.Cyan.dim(4), Color.Cyan.dim(3), Color.Cyan.dim(2))
}

case class RingParticle(ij:Cell, radius:Int, speed:Int, t:Int, r:Random) {
  val i = ij.x
  val j = ij.y
  import RingParticle._
  def update(tc:TerrainCache) = copy(t=t+1)
  def isDone = t > (radius+1)*speed
  def toParticle:Particle[RingParticle] = Particle(_.update, _.draw, _.getLight, _.isDone, this)
  def getLight = Seq(LightSource.createCircle((i, j), t/speed, t*2/speed, 1))
  def draw(tr:TileRenderer):TileRenderer = {

    val outer = t/speed
    val inner = scala.math.abs((t - speed*4)/speed.toFloat)
    val indices = for (p <- (i - outer) to (i + outer);
                       q <- (j - outer) to (j + outer)) yield {
      val h = scala.math.hypot(i - p, j - q)
      (p, q).onlyIf(h < outer && h > inner)
    }
    tr `$$>` (indices.flatten map { case (p, q) =>
      def f(t:Tile):Tile = {
        val bg = colors.randomR(r).dim(1 + r.nextDouble)
        val fg = colors.randomR(r).dim(1 + r.nextDouble)
        t.setFg(fg).setBg(bg)
      }
      ((p, q), f _)
    })
  }
}
