package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.world.TerrainCache

object Ladder {
  def create(ij:Cell, height:Int) = {
    val tile = CP437.╫.mkTile(Color.Black, Color.Brown)
    Ladder(ij, height, tile)
  }
}

case class Ladder(top:Cell, height:Int, tile:Tile) {

  def getPos = top
  def contains(ij:Cell) = Rope.between(ij, top.x, top.y, top.y + height)
  def isTop(ij:Cell) = ij == top
  def kill = this
  def isKillableAt(ij:Cell) = false
  def update(tc:TerrainCache) = this.some @@ Seq()
  def draw(tr:TileRenderer):TileRenderer = {
    tr <++ (0 until height).map{i => (top +| i, tile)}
  }

  def toClimbable = Climbable[Ladder](_.contains, _.isTop, _.isKillableAt, _.kill, _.update, _.draw, _.getPos, this)
}
