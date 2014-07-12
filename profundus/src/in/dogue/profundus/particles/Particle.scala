package in.dogue.profundus.particles

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Implicits
import Implicits._

case class Particle[T](up:T=>T,
                       drawFunc:T => TileRenderer => TileRenderer,
                       isDone:T => Boolean,
                       self:T) {
  def update:Option[Particle[_] forSome {type A}] = {
    if (isDone(self)) {
      None
    } else {
      copy(self=up(self)).some
    }
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< drawFunc(self)
  }
}
