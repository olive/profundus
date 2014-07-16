package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileRenderer, Animation}
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.world.WorldTile

object MineralPickup {
  def create(ij:Cell, c:Color) = {
    def anim = Animation.create(Vector(
      (5, CP437.●.mkTile(Color.Black, c)),
      (Int.MaxValue, CP437.`.`.mkTile(Color.Black, c))
    ))
    MineralPickup(ij, anim)
  }
}

case class MineralPickup private (ij:Cell, a:Animation) {

  def update = copy(a=a.update)

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+< a.drawFg(ij.x, ij.y)
  }

  def onPickup(pl:Player) = {
    pl.collectMineral(this)
  }
  def toPickup:Pickup[MineralPickup] = Pickup(ij, Grounded, _.update, _.onPickup, _.draw, this)
}
