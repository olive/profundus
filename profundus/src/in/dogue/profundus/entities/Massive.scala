package in.dogue.profundus.entities

import in.dogue.antiqua.data.Direction
import in.dogue.profundus.world.TerrainCache
import in.dogue.antiqua.Antiqua
import Antiqua._

case class Massive[T](pos: T => (Int,Int),
                      move: T => ((Int,Int)) => T,
                      setState: T => FallState => T,
                      state:FallState,
                      self:T) {
  def update(tc:TerrainCache) = {
    val epos = pos(self)
    val grounded = tc.isGrounded(epos)
    state match {
      case Floating => self
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
