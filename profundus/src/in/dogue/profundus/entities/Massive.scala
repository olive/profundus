package in.dogue.profundus.entities

import in.dogue.antiqua.data.Direction
import in.dogue.profundus.world.{WorldTile, TerrainCache}
import in.dogue.antiqua.Antiqua
import Antiqua._

case class Massive[T](pos: T => Cell,
                      move: T => (Cell, Direction, Direction => Option[WorldTile]) => T,
                      setState: T => FallState => T,
                      gMod: T => Double,
                      state: FallState,
                      self: T) {
  def update(tc:TerrainCache): T = {
    val epos = pos(self)
    if (!tc.isLoaded(epos)) {
      return self
    }
    val grounded = tc.isGrounded(epos)
    state match {
      case Floating => self
      case f@Falling(t, tiles) if !grounded =>
        val newT = (t + 1) % Falling.fallTime(gMod(self), tiles)
        val newTiles = tiles + (newT == 0).select(0, 1)
        val newPos = (newT == 0).select(epos, epos --> Direction.Down)
        val touching = tc.getTouching(newPos)
        val newState = Falling(newT, newTiles)
        val newE = move(self)(newPos, Direction.Down, touching)
        setState(newE)(newState)
      case _ if !grounded => setState(self)(Falling(0, 0))
      case _ => setState(self)(Grounded)
    }
  }
}
