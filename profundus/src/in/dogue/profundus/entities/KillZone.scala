package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._



case class KillZone[T](up:T => T,
                       isDone:T => Boolean,
                       cnts:T => ((Int,Int)) => Boolean,
                       self:T) {
  def update: Option[KillZone[A] forSome { type A }] = if (isDone(self)) {
    None
  } else {
    copy(self=up(self)).some
  }
  def contains(ij:(Int,Int)) = cnts(self)(ij)
}
