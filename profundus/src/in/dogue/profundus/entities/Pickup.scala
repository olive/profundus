package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.profundus.world.WorldTile
import in.dogue.antiqua.data.Direction


case class Pickup[T](ij:Cell,
                     fall:FallState,
                     up:T => T,
                     onPickup: T => Player => Player,
                     dr: T => Cell => TileRenderer => TileRenderer,
                     self:T) {
  def update = copy(self=up(self))

  def getPos = ij
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
