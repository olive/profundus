package in.dogue.profundus.entities.damagezones

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.{Damage, DamageType}

object ExplosionZone {
  def create(ij:Cell, radius:Int, speed:Int, source:DamageType) = {
    ExplosionZone(ij, radius, speed, source, 0)
  }
}
case class ExplosionZone private (pos:Cell, radius:Int, speed:Int, source:DamageType, t:Int) {
  def update = copy(t=t+1)
  def isDone = t > radius*speed
  def damagePerTick = Damage(10, source)
  def tickFreq = 1
  def contains(ij:Cell) = {
    scala.math.hypot(ij.x - pos.x, ij.y - pos.y) < t/speed
  }
  def toZone:DamageZone[ExplosionZone] = {
    DamageZone.create(_.update, _.isDone, _.damagePerTick, _.tickFreq, _.contains, this)
  }
}
