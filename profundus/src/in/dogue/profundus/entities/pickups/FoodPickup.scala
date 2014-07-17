package in.dogue.profundus.entities.pickups

import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.profundus.entities.Player
import in.dogue.antiqua.Antiqua
import Antiqua._

sealed trait FoodType {
  val seed:Int /** seed for random effects */
}
case class Toadstool(override val seed:Int) extends FoodType

object FoodPickup {
  val toadstool = CP437.τ.mkTile(Color.Black, Color.White)
  def create(ij:Cell, typ:FoodType) = {
    val tile = toadstool
    FoodPickup(ij, tile, typ)
  }

}
case class FoodPickup private (ij:Cell, a:Tile, typ:FoodType) {
  def update = this
  def onPickup(pl:Player) = {
    pl.collectFood(typ)
  }
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <| (ij.x, ij.y, a)
  }

  def toPickup:Pickup[FoodPickup] = Pickup.create[FoodPickup](ij, _.update, _.onPickup, _.draw, this)
}
