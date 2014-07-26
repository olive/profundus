package in.dogue.profundus.ui

import in.dogue.antiqua.graphics._
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.graphics.Tile
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.{Item, Capsule, Inventory}
import in.dogue.antiqua.graphics.Text
import in.dogue.profundus.Profundus
import scala.util.Random


object Hud {
  val gemIcon = CP437.♦.mkTile(Color.Black, Color.Cyan)
  val ropeIcon = CP437.⌡.mkTile(Color.Black, Color.Brown)
  val fuelIcon = CP437.f.mkTile(Color.Black, Color.Red)
  def create(cols:Int, rows:Int, inv:Inventory, stam:ValueBar, health:ValueBar):Hud = {
    val r = new Random(3)
    def mk(r:Random) = {
      val code = Vector(CP437.`:`, CP437.`;`).randomR(r)
      val bg = Color.Brown.dim(6 + r.nextDouble)
      val fg = Color.Brown.dim(3 + r.nextDouble)
      code.mkTile(bg, fg)
    }
    val blank = CP437.` `.mkTile(Color.Black, Color.White)
    val rect = Rect.createTextured(cols, rows, mk, r)
    val tf = Profundus.tf

    val tool = HudTool.create(ValueBar.create(inv.tool.`type`.durability, Color.White), tf, inv)
    Hud(cols, rect, inv, tool, stam, health, blank, tf.create("Dig down"), tf.create("Depth:"), tf.create("0"), Seq(), tf)
  }
}

case class Hud private (height:Int, rect:Rect,
                        inv:Inventory,
                        tool:HudTool,
                        stamBar:ValueBar, healthBar:ValueBar,
                        buffIcon:Tile,
                        text:Text, depth:Text, depthAmt:Text,
                        items:Seq[Item],
                        tf:TextFactory) {
  def atDepth(i:Int) = copy(depthAmt=tf.create("%4s".format(i.toString)))
  def withStam(s:ValueBar) = copy(stamBar=s)
  def withBuff(b:Tile) = copy(buffIcon=b)
  def withHealth(s:ValueBar) = copy(healthBar=s)
  def withItems(is:Seq[Item]) = copy(items=is)
  def withInventory(inv:Inventory) = {
    copy(inv=inv, tool = tool.withTool(inv.tool))
  }

  private def drawInventory(xy:Cell) (tr:TileRenderer):TileRenderer = {
    def fmt(i:Int) = tf.create("%2s".format(i.toString))
    val cap  = tr   <| (              xy, Capsule.stick) <+< fmt(inv.bombs).drawFg(xy |+ 1)
    val gem  = cap  <| (         xy +| 1, Hud.gemIcon)   <+< fmt(inv.minerals).drawFg( xy |+| ((1,1)))
    val rope = gem  <| (         xy |- 4, Hud.ropeIcon)  <+< fmt(inv.ropes).drawFg(xy |- 3)
               rope <| (xy |+| ((-4, 1)), Hud.fuelIcon)  <+< fmt(inv.fuel).drawFg( xy |+| ((-3, 1)))

  }

  private def drawDepth(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+< depth.drawFg(ij) <+< depthAmt.drawFg(ij |+ 7)
  }

  private def drawItems(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <|| items.zipWithIndex.map { case (item, k) =>
      (ij |+ k, item.icon)
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    (tr <+< rect.draw((0,0))
        <+< text.drawFg((1, 1))
        <+< drawDepth((1, 2))
        <+< drawInventory((28, 1))
        <+< tool.draw((13, 1))
        <+< stamBar.draw((13, 3)) <| ((12, 3), buffIcon)
        <+< healthBar.draw((13, 4))
        <+< drawItems((1, 4))
      )
  }
}
