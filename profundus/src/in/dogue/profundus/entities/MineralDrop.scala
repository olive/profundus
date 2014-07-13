package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileRenderer, Animation}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._

object MineralDrop {
  def create(ij:(Int,Int), c:Color) = {
    def anim = Animation.create(Vector(
      (5, CP437.●.mkTile(Color.Black, c)),
      (Int.MaxValue, CP437.`.`.mkTile(Color.Black, c))
    ))
    println("Spawned" + ij)
    MineralDrop(ij, anim, Grounded)
  }
}

case class MineralDrop private (ij:(Int,Int), a:Animation, fall:FallState) {
  def pos = ij
  def move(ij:(Int,Int)) = copy(ij=ij)

  def setState(f:FallState) = {
    copy(fall=f)
  }

  def update = copy(a=a.update)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< a.drawFg(ij.x, ij.y)
  }


  def toMassive:Massive[MineralDrop] = Massive(_.pos, _.move, _.setState, fall, this)
}
