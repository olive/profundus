package in.dogue.profundus.deformations

import in.dogue.profundus.world.World
import in.dogue.antiqua.Antiqua
import Antiqua._

case class Deformation[T](up:T => T,
                          done:T => Boolean,
                          deform:T=>World => World,
                          self:T) {
  def apply(w:World):World = deform(self)(w)

  def update:Option[Deformation[A] forSome {type A}] = {
    if (done(self)) {
      None
    } else {
      copy(self=up(self)).some
    }
  }

}
