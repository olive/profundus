package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileRenderer, TileFactory, Tile}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.antiqua.Antiqua
import Antiqua._
object ToolSprite {
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
    ToolSprite(tiles)
  }
}

case class ToolSprite private (tiles:Direction => Tile) {
  def draw(d:Direction)(ij:Cell)(tr:TileRenderer):TileRenderer = {
    val t = tiles(d)
    tr <+ (ij --> d, t)
  }
}
