package in.dogue.profundus.entities

import in.dogue.profundus.entities.pickups.{FoodPickup, Toadstool, FoodType}
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import in.dogue.antiqua.graphics.Tile
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._

sealed trait Buff {
  val duration:Int
  val icon:Tile = CP437.` `.mkTile(Color.Black, Color.White)
  def isDone:Boolean
  def update:Buff
  def process(attr:Attributes):Attributes
}

case object DeadBuff extends Buff {
  override val duration = Int.MaxValue
  override val icon = CP437.☻.mkTile(Color.Black, Color.Red)
  override def update = this
  override def isDone = false
  override def process(attr:Attributes) = {
    attr.copy(healthRegen = 0, stamRegen = 0)
  }
}

case class ToadstoolBuff(regenRate:Int, t:Int) extends Buff {
  override val icon = FoodPickup.toadstool
  override val duration = 60*60*5
  override def isDone = t > duration
  override def update = {
    if (!isDone) {
      copy(t = t + 1)
    } else {
      NoBuff
    }
  }

  override def process(attr:Attributes) = {
    attr.copy(stamRegen=regenRate)
  }

}

case class HerbBuff(regenRate:Int, t:Int) extends Buff {
  override val icon = FoodPickup.herb
  override val duration = 60*60*5
  override def isDone = t > duration
  override def update = {
    if (!isDone) {
      copy(t = t + 1)
    } else {
      NoBuff
    }
  }

  override def process(attr:Attributes) = {
    attr.copy(healthRegen=regenRate)
  }
}

case object NoBuff extends Buff {
  override val duration = 0
  override def isDone = false
  override def update = this

  override def process(attr:Attributes) = {
    val default = Attributes.default
    attr.copy(healthRegen = default.healthRegen, stamRegen = default.stamRegen, toolRegen = default.toolRegen)
  }
}

