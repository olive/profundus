package in.dogue.profundus.entities.pickups

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.profundus.world.{Unloadable, WorldTile}
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

  def apply[A](aij:Cell,
               afall:FallState,
               aup:A => A,
               acollectable:A => Player => Boolean,
               aonPickup: A => Player => Player,
               adr: A => Cell => TileRenderer => TileRenderer,
               aself:A) = new Pickup {

     override type T = A
     override val ij = aij
     override val fall = afall
     override val up = aup
     override val collectable = acollectable
     override val onPickup = aonPickup
     override val dr = adr
     override val self = aself
  }
}

trait Pickup   {
  type T
  val ij:Cell
  val fall:FallState
  val up:T => T
  val collectable:T => Player => Boolean
  val onPickup: T => Player => Player
  val dr: T => Cell => TileRenderer => TileRenderer
  val self:T

  private def copy(ij:Cell=ij,
                   fall:FallState=fall,
                   up:T => T=up,
                   collectable:T => Player => Boolean=collectable,
                   onPickup: T => Player => Player=onPickup,
                   dr: T => Cell => TileRenderer => TileRenderer=dr,
                   self:T=self) = {
    Pickup(ij, fall, up, collectable, onPickup, dr, self)
  }

  def update = copy(self=up(self))

  def getPos = ij
  def isCollectable(p:Player) = collectable(self)(p)
  def collect(p:Player):Player = onPickup(self)(p)

  def setState(s:FallState) = copy(fall=s)
  def move(ij:Cell, from:Direction, newTouching:Direction => Option[WorldTile]) = {
    copy(ij=ij)
  }
  def toMassive:Massive[Pickup] = Massive[Pickup](_.getPos, _.move, _.setState, fall, this)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< dr(self)(ij)
  }
  def toUnloadable = Unloadable.fromPos[Pickup](this, _.getPos)

}
