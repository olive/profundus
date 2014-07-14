package in.dogue.profundus.ui

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.graphics.{TileRenderer, TextFactory, Tile}
import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.entities.{Tool, Inventory, Shovel}

object HudTool {
  import Color._
  import CP437._


  val shovelBroken = Tile.makeGroup(Vector(
    (0, 0,  \ , Black, Brown),
    (1, 0, CP437.~, Black, Brown),
    (2, 0, CP437.~, Black, Brown),
    (3, 0, `)`, Black, Grey)
  ))


  def create(bar:DurabilityBar, tf:TextFactory, inv:Inventory) = {
    HudTool(bar, tf, inv.tool.`type`.icon)
  }
}

case class HudTool private (bar:DurabilityBar, tf:TextFactory, icon:TileGroup) {
  def withTool(tool:Tool) = {
    val d = tool.dura
    val max = tool.`type`.durability
    val newIcon = if (bar.isEmpty) {
      HudTool.shovelBroken
    } else {
      tool.`type`.icon
    }

    copy(bar=bar.update(d, max), icon=newIcon)
  }
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    def fmt(i:Int) = tf.create("%3s%%".format(i.toString))
    val percent = (100*bar.amt/bar.max.toFloat).toInt
    tr <++ (icon |+| (i, j)) <+< bar.draw(i, j+1) <+< fmt(percent).draw(i + 6, j)
  }

}
