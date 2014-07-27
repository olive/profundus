package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.ui.ValueBar
import in.dogue.antiqua.graphics.TileRenderer
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.entities.pickups.{Herb, Toadstool, FoodType}
import scala.util.Random

object StaminaBar {
  def create(max:Int) = StaminaBar(max, max, 0, ValueBar.create(max, Color.fromHsb(0.4,1.0,0.5)))
}

case class StaminaBar private (amt:Int, max:Int, t:Int, vb:ValueBar) {
  def update(attr:Attributes) = {
    val (newT, newAmt) = if (attr.stamRegen > 0 && t % attr.stamRegen == 0) {
      (1, (amt + 1).clamp(0, max))
    } else {
      (t+1, amt)
    }
    copy(t=newT, amt=newAmt, vb=vb.update(newAmt, max))
  }

  def setColor(c:Color) = {
    copy(vb=vb.setColor(c))
  }

  def remove(i:Int) = copy(amt=amt.drop(i), vb=vb.update(amt.drop(i), max))
  def removeAll = remove(amt)
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+< vb.draw(ij) <+< vb.drawEmpty(10, ij)
  }
}
