package in.dogue.profundus.entities.damagezones

import in.dogue.antiqua.Antiqua
import Antiqua._

object ExplosionZone {
  def create(i:Int, j:Int, radius:Int, speed:Int) = {
    ExplosionZone(i, j, radius, speed, 0)
  }
}
case class ExplosionZone private (i:Int, j:Int, radius:Int, speed:Int, t:Int) {
  def update = copy(t=t+1)
  def isDone = t > radius*speed
  def damagePerTick = 10
  def tickFreq = 1
  def contains(ij:Cell) = {
    scala.math.hypot(ij.x - i, ij.y - j) < t/speed
  }
  def toZone:DamageZone[ExplosionZone] = {
    DamageZone.create(_.update, _.isDone, _.damagePerTick, _.tickFreq, _.contains, this)
  }
}
