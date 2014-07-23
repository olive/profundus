package in.dogue.profundus.entities.damagezones

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.geometry.Circle
import in.dogue.profundus.entities.{DamageType, Damage}

case class DelayedExplosion(center:Cell, time:Int, duration:Int, radius:Int, t:Int) {
  def update = copy(t=t+1)
  def isDone = t > time + duration
  def dmgPerTick = Damage(100, DamageType.Phoebe)
  def tickFreq = 1
  def contains(c:Cell) = if (t > time ) {
    Circle(center, radius).contains(c)
  } else {
    false
  }
  def toZone = DamageZone[DelayedExplosion](_.update, _.isDone, _.dmgPerTick, _.tickFreq, _.contains, this, 0)
}
