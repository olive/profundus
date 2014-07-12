package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileRenderer, Animation}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Implicits
import Implicits._

object GemDrop {
  def create(i:Int, j:Int, c:Color) = {
    def anim = Animation.create(Vector(
      (5, CP437.●.mkTile(Color.Black, c)),
      (Int.MaxValue, CP437.`.`.mkTile(Color.Black, c))
    ))
    GemDrop(i, j, anim, Grounded)
  }
}

case class GemDrop private (i:Int, j:Int, a:Animation, fall:FallState) {
  def pos = (i, j)
  def move(ij:(Int,Int)) = copy(i=ij.x, j=ij.y)

  def setState(f:FallState) = {
    copy(fall=f)
  }

  def update = copy(a=a.update)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< a.drawFg(i, j)
  }

  def toMassive:Massive[GemDrop] = Massive(_.pos, _.move, _.setState, fall, this)
  //def toCollidable:Collidable[GemDrop] = Collidable(_.pos, )
}
