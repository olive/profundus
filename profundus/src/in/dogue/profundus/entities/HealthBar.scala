package in.dogue.profundus.entities

import in.dogue.profundus.ui.ValueBar
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Antiqua
import Antiqua._

object HealthBar {
  def create(max:Int) = HealthBar(max, max, max, 0, DamageType.Unknown, ValueBar.create(max, Color.Red))
}

case class HealthBar private (amt:Int, origMax:Int, max:Int, t:Int, last:DamageType, vb:ValueBar) {
  def update(attr:Attributes) = {
    val (newT, newAmt) = if (attr.healthRegen > 0 && t % attr.healthRegen == 0) {
      (1, (amt + 1).clamp(0, max))
    } else {
      (t+1, amt)
    }
    copy(t=newT, amt=newAmt, vb=vb.update(newAmt, origMax))
  }

  def setColor(c:Color) = {
    copy(vb=vb.setColor(c))
  }

  def remove(dmg:Damage) = {
    val i = dmg.amount
    drop(i).copy(last=dmg.source)
  }

  private def drop(i:Int) = {
    copy(amt=amt.drop(i), vb=vb.update(amt.drop(i), max))
  }
  def removeAll = drop(amt)
  def permaHurt(i:Int) = {
    copy(max=math.max(max.drop(i), 1))
  }
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< vb.draw(i, j)
  }
}
