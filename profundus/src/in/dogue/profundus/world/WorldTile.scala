package in.dogue.profundus.world

import in.dogue.antiqua.graphics.{TileRenderer, Tile}

sealed trait TileState
case object Empty extends TileState
case object Filled extends TileState

case class WorldTile(solid:Tile, empty:Tile, state:TileState) {
  def getTile = state match {
    case Empty => empty
    case Filled => solid
  }
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    val tile = getTile
    tr <+ (i, j, tile)
  }
}
