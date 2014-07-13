package in.dogue.profundus.world

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.entities.MineralDrop

sealed trait TileState {
  val tile:Tile
  val bg:Tile
  val isWalkable:Boolean = false
}

case class Empty(override val tile:Tile) extends TileState {
  override val isWalkable = true
  override val bg = tile
}
object Rock {
  def create(t:Tile, bg:Tile) = Rock(t, bg, 5)
}
case class Rock(override val tile:Tile, override val bg:Tile, hp:Int) extends TileState {
  def hit:(TileState, Seq[MineralDrop]) = if (hp > 1) {
    (copy(hp = hp.drop1), Seq())
  } else {
    (Empty(bg), Seq())
  }
}
object Dirt {
  def create(t:Tile, bg:Tile) = Dirt(t, bg, 1)
}
case class Dirt(override val tile:Tile, override val bg:Tile, hp:Int) extends TileState {
  def hit:(TileState, Seq[MineralDrop]) = {
    (Empty(bg), Seq())
  }
}
object Mineral {
  def create(t:Tile, bg:Tile, c:Color) = Mineral(t, bg, c, 3)
}
case class Mineral(override val tile:Tile, override val bg:Tile, c:Color, hp:Int) extends TileState {
  def hit(i:Int, j:Int):(TileState, Seq[MineralDrop]) = if (hp > 1) {
    (copy(hp=hp.drop1), Seq())
  } else {
    (Empty(bg), Seq(MineralDrop.create(i, j, c)))
  }
}

case class WorldTile(state:TileState) {
  def tile = state.tile
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    val tile = state.tile
    tr <+ (i, j, tile)
  }
}
