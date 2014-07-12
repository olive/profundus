package in.dogue.profundus.entities

import in.dogue.antiqua.data.Direction
import in.dogue.profundus.world.World
import in.dogue.antiqua.Implicits
import Implicits._

case class Massive[T](pos: T => (Int,Int),
                      move: T => ((Int,Int)) => T,
                      setState: T => FallState => T,
                      state:FallState,
                      self:T) {
  def update(w:World) = {
    val epos = pos(self)
    val grounded = w.isGrounded(epos)
    state match {
      case f@Falling(t, tiles) if !grounded =>
        val newT = (t + 1) % f.fallTime
        val newTiles = tiles + (newT == 0).select(0, 1)
        val newPos = (newT == 0).select(epos, epos --> Direction.Down)
        val newState = Falling(newT, newTiles)
        val newE = move(self)(newPos)
        setState(newE)(newState)
      case _ if !grounded => setState(self)(Falling(0, 0))
      case _ => setState(self)(Grounded)
    }
  }
}
