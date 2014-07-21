package in.dogue.profundus.particles

import in.dogue.antiqua.Antiqua
import Antiqua._

case class Emitter[T](up:T => (T, Seq[Particle[_]]),
                      done: T => Boolean,
                      self:T) {
  def update:(Option[(Emitter[T], Seq[Particle[R] forSome {type R}])]) = {
    if (isDone) {
      None
    } else {
      val (e, ps) = up(self)
      (copy(self=e), ps).some
    }
  }

  def isDone:Boolean = done(self)
}
