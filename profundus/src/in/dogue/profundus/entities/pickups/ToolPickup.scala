package in.dogue.profundus.entities.pickups

import in.dogue.profundus.entities.{Tool, Player, Grounded}
import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._

object ToolPickup {
  def create(ij:Cell, tool:Tool) = {
    val icon = CP437./.mkTile(Color.Black, Color.Brown)
    ToolPickup(icon, tool).toPickup(ij)
  }
}

case class ToolPickup(icon:Tile, tool:Tool) {
  def update = this
  def isCollectable(p:Player) = p.inv.tool.isBare
  def onPickup(p:Player) = p.collectTool(tool)
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <| (ij, icon)
  }
  def toPickup(ij:Cell):Pickup = Pickup.create[ToolPickup](ij, _.update, _.isCollectable, _.onPickup, _.draw, this)
}
