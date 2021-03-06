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


  def create(bar:ValueBar, tf:TextFactory, inv:Inventory) = {
    HudTool(bar, tf, inv.tool.`type`.icon)
  }
}

case class HudTool private (bar:ValueBar, tf:TextFactory, icon:TileGroup) {
  def withTool(tool:Tool) = {
    val d = tool.dura
    val max = tool.`type`.durability
    val newIcon =  tool.`type`.icon

    copy(bar=bar.update(d, max), icon=newIcon)
  }
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    def fmt(i:Int) = tf.create("%3s%%".format(i.toString))
    val percent = (100*bar.amt/bar.max.toFloat).toInt
    tr <|| (icon |++| ij) <+< bar.draw(ij +| 1) <+< fmt(percent).drawFg(ij |+ 6)
  }

}
