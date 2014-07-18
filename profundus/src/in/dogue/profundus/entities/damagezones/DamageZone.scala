package in.dogue.profundus.entities.damagezones

import in.dogue.antiqua.Antiqua._


object DamageZone {
  def create[T](up:T => T,
                isDone:T => Boolean,
                damagePerTick:T => Int,
                tickFreq:T => Int,
                cnts:T => Cell => Boolean,
                self:T) = {
    DamageZone(up, isDone, damagePerTick, tickFreq, cnts, self, 0)
  }

  def process[T](kz:Seq[DamageZone[_]], t:T, apply:Int => T, pos:Cell):T = {
    kz.foldLeft(t) { case (tt, z) =>
      if (z.contains(pos)) {
        apply(z.getDamage)
      } else {
        tt
      }
    }
  }
}
case class DamageZone[T] private (up:T => T,
                                  isDone:T => Boolean,
                                  damagePerTick:T => Int,
                                  tickFreq:T => Int,
                                  cnts:T => Cell => Boolean,
                                  self:T,
                                  t:Int) {
  def update: Option[DamageZone[A] forSome { type A }] = if (isDone(self)) {
    None
  } else {
    copy(self=up(self), t=t+1).some
  }

  def getDamage = {
    val freq = tickFreq(self)
    if (freq <= 1 || (t > 0 && t % tickFreq(self) == 0)) {
      damagePerTick(self)
    } else {
      0
    }
  }



  def contains(ij:Cell) = cnts(self)(ij)
}
