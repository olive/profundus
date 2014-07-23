package in.dogue.profundus.entities.damagezones

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.entities.{Damage, DamageType}

case class SingleTileZone(ij:Cell, dmgPerTick:Int, source:DamageType) {
  def update = this
  def isDone = true
  def damagePerTick = Damage(dmgPerTick, source)
  def tickFreq = 1
  def contains(pq:Cell) = ij == pq
  def toZone:DamageZone = DamageZone[SingleTileZone](_.update, _.isDone, _.damagePerTick, _.tickFreq, _.contains, this, 0)
}
