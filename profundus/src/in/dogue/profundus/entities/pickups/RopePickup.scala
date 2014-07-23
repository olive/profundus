package in.dogue.profundus.entities.pickups

import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.entities.{Grounded, Player}


object RopePickup {
  def create(ij:Cell) = {
    val t = CP437.`@`.mkTile(Color.Black, Color.White)
    RopePickup(ij, t)
  }
}

case class RopePickup private (ij:Cell, t:Tile) {

  def update = this

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <| (ij, t)
  }

  def isCollectable(p:Player) = true

  def onPickup(pl:Player) = {
    pl.collectRope(this)
  }
  def toPickup:Pickup = Pickup.create[RopePickup](ij, _.update, _.isCollectable, _.onPickup, _.draw, this)
}
