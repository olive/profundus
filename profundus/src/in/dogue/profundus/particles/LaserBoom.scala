package in.dogue.profundus.particles

import in.dogue.profundus.world.TerrainCache
import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.geometry.Line
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.audio.SoundManager
import scala.util.Random

object LaserBoom {
  def create(src:Cell, dst:Cell, speed:Int) = {
    val tile = CP437.`·`.mkTile(Color.Black, Color.White)
    LaserBoom(Line.bresenhamTup(src, dst).drop(1).toVector, tile, speed, 0)
  }
}

case class LaserBoom private (poses:Vector[Cell], tile:Tile, speed:Int, t:Int) {
  def update(tc:TerrainCache) = {
    if (t == speed* poses.length) {
      poses.lastOption.foreach { cell =>
        SoundManager.pow.play(cell)
      }

    }
    copy(t=t+1)
  }

  def timeToExplode = speed*poses.length

  private def getFrame:(Cell, Tile) = (poses((t / speed).clamp(0, poses.length - 1)), tile)
  def draw(tr:TileRenderer):TileRenderer = {
    if (t < speed* poses.length) {
      tr <+~ getFrame
    } else {
      val colors = Vector(Color.Green, Color.DarkGreen.dim(2), Color.DarkGreen)
      val i = poses.last.x
      val j = poses.last.y
      val tt = t - speed * poses.length
      val outer = tt/speed
      val inner = scala.math.abs((tt - speed*4)/speed.toFloat)
      val indices = for (p <- (i - outer) to (i + outer);
                         q <- (j - outer) to (j + outer)) yield {
        val h = scala.math.hypot(i - p, j - q)
        (p, q).onlyIf(h < outer && h > inner)
      }
      val r = new Random()
      tr `$$>` (indices.flatten map { case pos =>
        def f(t:Tile):Tile = {
          val bg = colors.randomR(r).dim(0.2 + r.nextDouble)
          val fg = colors.randomR(r).dim(0.2 + r.nextDouble)
          t.setFg(fg).setBg(bg)
        }
        (pos, f _)
      })
    }
  }
  def getLight = Seq()
  def isDone = t > speed * poses.length * 2
  def toParticle = Particle[LaserBoom](_.update, _.draw, _.getLight, _.isDone, this)
}
