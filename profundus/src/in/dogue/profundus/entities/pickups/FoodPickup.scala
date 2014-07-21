package in.dogue.profundus.entities.pickups

import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.profundus.entities.Player
import in.dogue.antiqua.Antiqua
import Antiqua._
import scala.util.Random

object FoodType {
  def All(r:Random) = {
    val seed = r.nextInt.abs
    Vector(Toadstool(seed), Herb(seed), Bark(seed))
  }
  def random(r:Random) = {
    All(r).randomR(r)
  }
}

sealed trait FoodType {
  val seed:Int /** seed for random effects */
}
case class Toadstool(override val seed:Int) extends FoodType
case class Herb(override val seed:Int) extends FoodType
case class Bark(override val seed:Int) extends FoodType

object FoodPickup {
  val toadstool = CP437.τ.mkTile(Color.Black, Color.White)
  val herb = CP437.♣.mkTile(Color.Black, Color.DarkGreen)
  val bark = CP437.~.mkTile(Color.Black, Color.Brown)
  def create(ij:Cell, typ:FoodType) = {
    val tile = typ match {
      case Toadstool(_) => toadstool
      case Herb(_) => herb
      case Bark(_) => bark
    }
    FoodPickup(ij, tile, typ)
  }

}
case class FoodPickup private (ij:Cell, a:Tile, typ:FoodType) {
  def update = this

  def isCollectable(p:Player) = true
  def onPickup(pl:Player) = {
    pl.collectFood(typ)
  }
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <| (ij, a)
  }

  def toPickup:Pickup[FoodPickup] = Pickup.create[FoodPickup](ij, _.update, _.isCollectable, _.onPickup, _.draw, this)
}
