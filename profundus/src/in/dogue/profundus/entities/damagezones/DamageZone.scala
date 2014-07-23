package in.dogue.profundus.entities.damagezones

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.entities.{DamageType, Damage}


object DamageZone {
  def apply[A](aup:A => A,
               aisDone:A => Boolean,
               adamagePerTick:A => Damage,
               atickFreq:A => Int,
               acnts:A => Cell => Boolean,
               aself:A,
               at:Int) = new DamageZone {
    override type T = A
    override val up = aup
    override val isDone = aisDone
    override val damagePerTick = adamagePerTick
    override val tickFreq = atickFreq
    override val cnts = acnts
    override val self = aself
    override val t = at
  }

  def process[T](kz:Seq[DamageZone], t:T, apply:Damage => T, pos:Cell):T = {
    kz.foldLeft(t) { case (tt, z) =>
      if (z.contains(pos)) {
        z.getDamage.map { d => apply(d) }.getOrElse(tt)
      } else {
        tt
      }
    }
  }
}
trait DamageZone {
  type T
  val up:T => T
  val isDone:T => Boolean
  val damagePerTick:T => Damage
  val tickFreq:T => Int
  val cnts:T => Cell => Boolean
  val self:T
  val t:Int

  def updateSelf = DamageZone(up, isDone, damagePerTick, tickFreq, cnts, up(self), t+1)

  def update: Option[DamageZone] = if (isDone(self)) {
    None
  } else {
    updateSelf.some
  }

  def getDamage = {
    val freq = tickFreq(self)
    if (freq <= 1 || (t > 0 && t % tickFreq(self) == 0)) {
      damagePerTick(self).some
    } else {
      None
    }
  }



  def contains(ij:Cell) = cnts(self)(ij)
}
