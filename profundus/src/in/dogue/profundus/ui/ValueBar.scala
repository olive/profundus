package in.dogue.profundus.ui

import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua
import Antiqua._
import scala.math._

object ValueBar {
  def create(max:Int, c:Color) = {
    val tFull = CP437.█.mkTile(Color.Black, c)
    val tHalf = CP437.▌.mkTile(Color.Black, c)
    ValueBar(max, max, tFull, tHalf)
  }
}
case class ValueBar private (amt:Int, max:Int, tFull:Tile, tHalf:Tile) {
  final val maxWidth = 10
  def setColor(c:Color) = copy(tFull=tFull.setFg(c), tHalf=tHalf.setFg(c))
  def isEmpty = amt == 0

  def update(amt:Int, max:Int) = {
    copy(amt=amt, max=max)
  }
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    import scala.math.ceil

    val numTiles = ceil((amt/max.toFloat)*2*maxWidth).toInt
    val half = ceil(numTiles/2.0).toInt
    val draws = for (p <- 0 until half) yield {
      val tile = if (p == half - 1 && numTiles % 2 == 1) {
        tHalf
      } else {
        tFull
      }
      (ij |+ p, tile)
    }

    tr <++ draws
  }

  def drawEmpty(maxWidth:Int, ij:Cell)(tr:TileRenderer):TileRenderer = {
    val numTiles = ceil((amt/max.toFloat)*maxWidth).toInt
    val half = ceil(numTiles).toInt
    val draws = for (p <- half until maxWidth) yield {
      (ij |+ p, tFull.setFg(Color.Black))
    }
    tr <++ draws
  }
}
