package in.dogue.profundus.ui

import in.dogue.antiqua.graphics._
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.graphics.Tile
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.{Capsule, Inventory}
import in.dogue.antiqua.graphics.Text
import in.dogue.profundus.Profundus


object Hud {
  val gemIcon = CP437.♦.mkTile(Color.Black, Color.Cyan)
  val ropeIcon = CP437.⌡.mkTile(Color.Black, Color.Brown)
  val fuelIcon = CP437.f.mkTile(Color.Black, Color.Red)
  def create(cols:Int, rows:Int, inv:Inventory):Hud = {
    val rect = Rect.createPlain(cols, rows, CP437.` `.mkTile(Color.Black, Color.White))
    val tf = Profundus.tf
    val shovel = HudTool.create(DurabilityBar.create(inv.tool.`type`.durability), tf, inv)
    Hud(cols, rect, inv, shovel, tf.create("Dig down"), tf.create("Depth:"), tf.create("0"), tf)
  }
}

case class Hud private (height:Int, rect:Rect,
                        inv:Inventory,
                        tool:HudTool,
                        text:Text, depth:Text, depthAmt:Text,
                        tf:TextFactory) {
  def atDepth(i:Int) = copy(depthAmt=tf.create(i.toString))
  def withInventory(inv:Inventory) = {
    copy(inv=inv, tool = tool.withTool(inv.tool))
  }

  private def drawInventory(tr:TileRenderer):TileRenderer = {
    val x = 27
    val y = 1
    def fmt(i:Int) = tf.create("%2s".format(i.toString))
    val cap  = tr <+ (x, y, Capsule.stick) <+< fmt(inv.bombs).draw(x + 1, y)
    val gem  = cap <+ (x, y+1, Hud.gemIcon) <+< fmt(inv.gems).draw(x + 1, y + 1)
    val rope = gem <+ (x - 4, y, Hud.ropeIcon) <+< fmt(inv.ropes).draw(x - 3, y)
               rope <+ (x - 4, y + 1, Hud.fuelIcon) <+< fmt(inv.fuel).draw(x - 3, y + 1)

  }

  private def drawDepth(tr:TileRenderer):TileRenderer = {
    tr <+< depth.draw(1, 2) <+< depthAmt.draw(8, 2)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< rect.draw(0,0) <+< text.draw(1, 1) <+< drawDepth <+< drawInventory <+< tool.draw(12, 1)
  }
}
