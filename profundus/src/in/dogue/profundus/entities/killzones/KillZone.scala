package in.dogue.profundus.entities.killzones

import in.dogue.antiqua.Antiqua._



case class KillZone[T](up:T => T,
                       isDone:T => Boolean,
                       cnts:T => Cell => Boolean,
                       self:T) {
  def update: Option[KillZone[A] forSome { type A }] = if (isDone(self)) {
    None
  } else {
    copy(self=up(self)).some
  }
  def contains(ij:Cell) = cnts(self)(ij)
}
