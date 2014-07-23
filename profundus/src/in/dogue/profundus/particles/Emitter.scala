package in.dogue.profundus.particles

import in.dogue.antiqua.Antiqua
import Antiqua._


object Emitter {
  def apply[A](aup:A => (A, Seq[Particle]),
               adone: A => Boolean,
               aself:A) = new Emitter {
    override type T = A
    override val up = aup
    override val done = adone
    override val self = aself
  }
}

trait Emitter {
  type T
  val up:T => (T, Seq[Particle])
  val done: T => Boolean
  val self:T

  private def updateSelf(ns:T) = Emitter(up, done, ns)

  def update:(Option[(Emitter, Seq[Particle])]) = {
    if (isDone) {
      None
    } else {
      val (e, ps) = up(self)
      (updateSelf(e), ps).some
    }
  }

  def isDone:Boolean = done(self)
}
