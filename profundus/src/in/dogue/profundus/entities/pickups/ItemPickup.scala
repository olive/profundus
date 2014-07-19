package in.dogue.profundus.entities.pickups

import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.graphics.{TileRenderer, Animation}
import in.dogue.profundus.entities.{Item, Player}


case class ItemPickup(ij:Cell, it:Item) {

  def update = this

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+ (ij.x, ij.y, it.icon)
  }

  def onPickup(pl:Player) = {
    pl.collectItem(it)
  }
  def toPickup:Pickup[ItemPickup] = Pickup.create(ij, _.update, _.onPickup, _.draw, this)

}