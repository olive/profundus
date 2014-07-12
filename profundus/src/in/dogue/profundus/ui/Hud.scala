package in.dogue.profundus.ui

import in.dogue.antiqua.graphics._
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.graphics.Text
import in.dogue.antiqua.graphics.Tile
import in.dogue.antiqua.Implicits
import Implicits._
import in.dogue.profundus.entities.{Capsule, Inventory}

object Hud {
  val gemIcon = CP437.♦.mkTile(Color.Black, Color.Cyan)
  def create(cols:Int, rows:Int, inv:Inventory):Hud = {
    val rect = Rect.createPlain(cols, rows, CP437.` `.mkTile(Color.Black, Color.White))
    val tf = TextFactory(Color.Black, Color.White, CP437.unicodeToCode)
    Hud(cols, rect, inv, tf.create("Dig down"), tf.create("Depth:"), tf.create("0"), tf)
  }
}

case class Hud private (height:Int, rect:Rect,
                        inv:Inventory,
                        text:Text, depth:Text, depthAmt:Text,
                        tf:TextFactory) {
  def atDepth(i:Int) = copy(depthAmt=tf.create(i.toString))
  def withInventory(inv:Inventory) = {
    copy(inv=inv)
  }

  private def drawInventory(tr:TileRenderer):TileRenderer = {
    val x = 28
    val y = 1
    val cap = tr <+ (x, y, Capsule.stick) <+< tf.create(inv.bombs.toString).draw(x + 2, y)
              cap <+ (x, y+1, Hud.gemIcon) <+< tf.create(inv.gems.toString).draw(x + 2, y + 1)
  }

  private def drawDepth(tr:TileRenderer):TileRenderer = {
    tr <+< depth.draw(1, 2) <+< depthAmt.draw(8, 2)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< rect.draw(0,0) <+< text.draw(1, 1) <+< drawDepth <+< drawInventory
  }
}
