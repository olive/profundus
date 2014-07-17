package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.ui.ValueBar
import in.dogue.antiqua.graphics.TileRenderer
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.entities.pickups.FoodType

object StaminaBar {
  def create(max:Int, speed:Int) = StaminaBar(max, max, speed, 0, ValueBar.create(max, Color.Green))
}

case class StaminaBar private (amt:Int, max:Int, regenSpeed:Int, t:Int, vb:ValueBar) {
  def update = {
    val (newT, newAmt) = if (t % regenSpeed == 0) {
      (1, (amt + 1).clamp(0, max))
    } else {
      (t+1, amt)
    }
    copy(t=newT, amt=newAmt, vb=vb.update(newAmt, max))
  }

  def eatFood(food:FoodType):StaminaBar = {
    println("eaten")
    this
  }

  def remove(i:Int) = copy(amt=amt.drop(i), vb=vb.update(amt.drop(i), max))


  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< vb.draw(i, j)
  }
}
