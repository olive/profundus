package in.dogue.profundus.particles

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import scala.util.Random
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.world.TerrainCache

object ExplosionParticle {
  def create(ij:Cell, radius:Int, speed:Int) = {
    ExplosionParticle(ij, radius, speed, 0, new Random())
  }
  private final val colors = Vector(Color.Yellow, Color.Orange, Color.Red)
}
case class ExplosionParticle private (ij:Cell, radius:Int, speed:Int, t:Int, r:Random) {
  final val i = ij.x
  final val j = ij.y
  import ExplosionParticle._
  def update(tc:TerrainCache) = copy(t=t+1)
  def isDone = t > (radius+1)*speed
  def toParticle:Particle[ExplosionParticle] = Particle(_.update, _.draw, _.getLight, _.isDone, this)
  def getLight = Seq(LightSource.createCircle(ij, t/speed, t*2/speed, 1))
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
        val bg = colors.randomR(r).dim(0.2 + r.nextDouble)
        val fg = colors.randomR(r).dim(0.2 + r.nextDouble)
        t.setFg(fg).setBg(bg)
      }
      ((p, q), f _)
    })
  }
}
