package in.dogue.profundus.world

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Implicits
import Implicits._
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.entities.MineralDrop

sealed trait TileState
case object Empty extends TileState
case object Filled extends TileState
object Mineral {
  def create(c:Color) = Mineral(c, 3)
}
case class Mineral(c:Color, hp:Int) extends TileState {
  def hit(i:Int, j:Int) = if (hp > 1) {
    (copy(hp=hp.drop1), Seq())
  } else {
    (Empty, Seq(MineralDrop.create(i, j, c)))
  }
}

case class WorldTile(solid:Tile, empty:Tile, gem:Tile, state:TileState) {
  def getTile = state match {
    case Empty => empty
    case Filled => solid
    case Mineral(_,_) => gem
  }
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    val tile = getTile
    tr <+ (i, j, tile)
  }
}
