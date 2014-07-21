package in.dogue.profundus.entities.pickups

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.profundus.world.WorldTile
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.entities.{Grounded, Massive, Player, FallState}


object Pickup {
  def create[T](ij:Cell,
               up:T => T,
               collectable:T => Player => Boolean,
               onPickup: T => Player => Player,
               dr: T => Cell => TileRenderer => TileRenderer,
               self:T) = {
    Pickup(ij, Grounded, up, collectable, onPickup, dr, self)
  }
}

case class Pickup[T] private (ij:Cell,
                              fall:FallState,
                              up:T => T,
                              collectable:T => Player => Boolean,
                              onPickup: T => Player => Player,
                              dr: T => Cell => TileRenderer => TileRenderer,
                              self:T) {
  def update = copy(self=up(self))

  def getPos = ij
  def isCollectable(p:Player) = collectable(self)(p)
  def collect(p:Player):Player = onPickup(self)(p)

  def setState(s:FallState) = copy(fall=s)
  def move(ij:Cell, from:Direction, newTouching:Direction => Option[WorldTile]) = {
    copy(ij=ij)
  }
  def toMassive:Massive[Pickup[_]] = Massive(_.getPos, _.move, _.setState, fall, this)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< dr(self)(ij)
  }

}
