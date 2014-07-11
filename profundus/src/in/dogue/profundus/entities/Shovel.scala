package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileRenderer, TileFactory, Tile}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.antiqua.Implicits
import Implicits._
object Shovel {
  def create = {
    import Direction._
    def tiles(d:Direction) = {
      val code = d match {
        case Up => CP437.^
        case Down => CP437.v
        case Left => CP437.<
        case Right => CP437.>
      }
      code.mkTile(Color.Black, Color.White)
    }
    Shovel(tiles)
  }
}

case class Shovel private (tiles:Direction => Tile) {
  def draw(d:Direction)(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    val x = i + d.dx
    val y = j + d.dy
    val t = tiles(d)
    tr <+ (x, y, t)
  }
}
