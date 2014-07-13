package in.dogue.profundus.ui

import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua
import Antiqua._
object DurabilityBar {
  def create(max:Int) = {
    val tFull = CP437.█.mkTile(Color.Black, Color.White)
    val tHalf = CP437.▌.mkTile(Color.Black, Color.White)
    DurabilityBar(max, max, tFull, tHalf)
  }
}
case class DurabilityBar private (amt:Int, max:Int, tFull:Tile, tHalf:Tile) {

  def isEmpty = amt == 0

  def update(amt:Int, max:Int) = {
    copy(amt=amt, max=max)
  }
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    import scala.math.ceil
    val maxWidth = 10
    val numTiles = ceil((amt/max.toFloat)*2*maxWidth).toInt
    val half = ceil(numTiles/2.0).toInt
    val draws = for (p <- 0 until half) yield {
      val tile = if (p == half - 1 && numTiles % 2 == 1) {
        tHalf
      } else {
        tFull
      }
      (i+p, j, tile)
    }
    tr <++ draws
  }
}
