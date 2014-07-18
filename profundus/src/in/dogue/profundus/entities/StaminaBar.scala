package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.ui.ValueBar
import in.dogue.antiqua.graphics.TileRenderer
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.entities.pickups.{Herb, Toadstool, FoodType}
import scala.util.Random

object StaminaBar {
  def create(max:Int, speed:Int) = StaminaBar(max, max, speed, 0, ValueBar.create(max, Color.Green), NoBuff(speed))
}

case class StaminaBar private (amt:Int, max:Int, regenSpeed:Int, t:Int, vb:ValueBar, buff:StaminaBuff) {
  def update = {
    val (newT, newAmt) = if (t % regenSpeed == 0) {
      (1, (amt + 1).clamp(0, max))
    } else {
      (t+1, amt)
    }
    buff.process(copy(t=newT, amt=newAmt, vb=vb.update(newAmt, max), buff=buff.update))
  }

  def setColor(c:Color) = {
    copy(vb=vb.setColor(c))
  }

  def remove(i:Int) = copy(amt=amt.drop(i), vb=vb.update(amt.drop(i), max))
  def eatFood(t:FoodType) = {
    t match {
      case Toadstool(seed) =>
        val speed = 1 + new Random(seed).nextInt(2*regenSpeed)
        val b = ToadstoolBuff(regenSpeed, speed, 0)
        copy(buff=b)
      case Herb(seed) => this
    }
  }

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< vb.draw(i, j)
  }
}
