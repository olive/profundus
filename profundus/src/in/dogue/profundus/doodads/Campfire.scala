package in.dogue.profundus.doodads

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.lighting.LightSource
import in.dogue.antiqua.graphics.{TileRenderer, Animation}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color

object Campfire {

  def create(ij:Cell) = {
    val c = CP437.underscore
    val bg = Color.Black
    val anim = Animation.create(Vector(
      (3, c.mkTile(bg, Color.Red)),
      (3, c.mkTile(bg, Color.Orange)),
      (3, c.mkTile(bg, Color.Yellow))
    ))
    val light = LightSource.createCircle(ij, 5, 10, 0.7)
    Campfire(ij, anim, light, 0)
  }
}

case class Campfire(ij:Cell, a:Animation, light:LightSource, t:Int) {
  def update = copy(a=a.update, t=t+1)

  def getLight:Option[LightSource] = light.copy(flicker=LightSource.flicker(11111+t)).some
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< a.drawFg(ij)
  }

  def toDoodad:Doodad[Campfire] = Doodad(_.update, _.draw, _.getLight, this)
}
