package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileRenderer, TileFactory, Tile}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.{Direction, CP437}

object Shovel {
  def create = {
    import Direction._
    val f = TileFactory(Color.Black, Color.White)
    def tiles(d:Direction) = d match {
      case Up => f(CP437.^)
      case Down => f(CP437.v)
      case Left => f(CP437.<)
      case Right => f(CP437.>)
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
