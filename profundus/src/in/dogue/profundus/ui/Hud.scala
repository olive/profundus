package in.dogue.profundus.ui

import in.dogue.antiqua.graphics._
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.{Code, CP437}
import in.dogue.antiqua.graphics.Text
import in.dogue.antiqua.graphics.Tile
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.{Capsule, Inventory}
import in.dogue.antiqua.graphics.Text

object DurabilityBar {
  def create(max:Int) = {
    val tFull = CP437.█.mkTile(Color.Black, Color.White)
    val tHalf = CP437.▌.mkTile(Color.Black, Color.White)
    DurabilityBar(max, max, tFull, tHalf)
  }
}
case class DurabilityBar private (amt:Int, max:Int, tFull:Tile, tHalf:Tile) {

  def isEmpty = amt == 0

  def update(amt:Int, max:Int) = {
    copy(amt=amt, max=max)
  }
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    import scala.math.ceil
    val maxWidth = 10
    val numTiles = ceil((amt/max.toFloat)*2*maxWidth).toInt
    val half = ceil(numTiles/2.0).toInt
    val draws = for (p <- 0 until half) yield {
      val tile = if (p == half - 1 && numTiles % 2 == 1) {
        tHalf
      } else {
        tFull
      }
      (i+p, j, tile)
    }
    tr <++ draws
  }
}

object HudShovel {
  import Color._
  import CP437._
  val shovel = Tile.makeGroup(Vector(
    (0, 0, `[`, Black, Brown),
    (1, 0, `─`, Black, Brown),
    (2, 0, `─`, Black, Brown),
    (3, 0, `D`, Black, Grey)
  ))

  val broken = Tile.makeGroup(Vector(
    (0, 0,  \ , Black, Brown),
    (1, 0, `X`, Black, Red),
    (2, 0, `X`, Black, Red),
    (3, 0, `)`, Black, Grey)
  ))

  def create(bar:DurabilityBar, tf:TextFactory) = {
    HudShovel(bar, tf, shovel)
  }
}

case class HudShovel private (bar:DurabilityBar, tf:TextFactory, icon:TileGroup) {
  def withDura(d:Int, max:Int) = {
    val newIcon = if (bar.isEmpty) {
      HudShovel.broken
    } else {
      icon
    }

    copy(bar=bar.update(d, max), icon=newIcon)
  }
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    def fmt(i:Int) = tf.create("%3s%%".format(i.toString))
    val percent = (100*bar.amt/bar.max.toFloat).toInt
    tr <++ (icon |+| (i, j)) <+< bar.draw(i, j+1) <+< fmt(percent).draw(i + 6, j)
  }

}

object Hud {
  val gemIcon = CP437.♦.mkTile(Color.Black, Color.Cyan)
  def create(cols:Int, rows:Int, inv:Inventory):Hud = {
    val rect = Rect.createPlain(cols, rows, CP437.` `.mkTile(Color.Black, Color.White))
    val tf = TextFactory(Color.Black, Color.White, CP437.unicodeToCode)
    val shovel = HudShovel.create(DurabilityBar.create(inv.maxDura), tf)
    Hud(cols, rect, inv, shovel, tf.create("Dig down"), tf.create("Depth:"), tf.create("0"), tf)
  }
}

case class Hud private (height:Int, rect:Rect,
                        inv:Inventory,
                        shovel:HudShovel,
                        text:Text, depth:Text, depthAmt:Text,
                        tf:TextFactory) {
  def atDepth(i:Int) = copy(depthAmt=tf.create(i.toString))
  def withInventory(inv:Inventory) = {
    copy(inv=inv, shovel = shovel.withDura(inv.dura, inv.maxDura))
  }

  private def drawInventory(tr:TileRenderer):TileRenderer = {
    val x = 27
    val y = 1
    def fmt(i:Int) = tf.create("%2s".format(i.toString))
    val cap = tr <+ (x, y, Capsule.stick) <+< fmt(inv.bombs).draw(x + 1, y)
              cap <+ (x, y+1, Hud.gemIcon) <+< fmt(inv.gems).draw(x + 1, y + 1)
  }

  private def drawDepth(tr:TileRenderer):TileRenderer = {
    tr <+< depth.draw(1, 2) <+< depthAmt.draw(8, 2)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< rect.draw(0,0) <+< text.draw(1, 1) <+< drawDepth <+< drawInventory <+< shovel.draw(12, 1)
  }
}
