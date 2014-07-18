package in.dogue.profundus.entities.damagezones

import in.dogue.antiqua.Antiqua._

case class SingleTileZone(ij:Cell, dmgPerTick:Int) {
  def update = this
  def isDone = true
  def damagePerTick = dmgPerTick
  def tickFreq = 1
  def contains(pq:Cell) = ij == pq
  def toZone:DamageZone[SingleTileZone] = DamageZone.create(_.update, _.isDone, _.damagePerTick, _.tickFreq, _.contains, this)
}
