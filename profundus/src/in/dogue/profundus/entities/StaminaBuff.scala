package in.dogue.profundus.entities

import in.dogue.profundus.entities.pickups.{FoodPickup, Toadstool, FoodType}
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import in.dogue.antiqua.graphics.Tile
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._

sealed trait StaminaBuff {
  val duration:Int

  val icon:Tile = CP437.` `.mkTile(Color.Black, Color.White)
  def isDone:Boolean
  def update:StaminaBuff
  def process(bar:StaminaBar):StaminaBar
}
case class ToadstoolBuff(oldRegen:Int, regenRate:Int, t:Int) extends StaminaBuff {
  val color = Color.Tan
  override val icon = FoodPickup.toadstool
  override val duration = 60*60*5
  override def isDone = t > duration
  override def update = {
    if (!isDone) {
      copy(t = t + 1)
    } else {
      //not here
      NoBuff(oldRegen)
    }
  }
  //never called
  override def process(bar:StaminaBar) = {
    bar.copy(regenSpeed = regenRate).setColor(color)
  }

}

case class NoBuff(regenRate:Int) extends StaminaBuff {
  override val duration = 0
  override def isDone = false
  override def update = this
  //not here
  override def process(bar:StaminaBar) = bar.copy(regenSpeed = regenRate).setColor(Color.Green)
}

