package in.dogue.profundus.entities

import in.dogue.antiqua.Implicits
import Implicits._
import in.dogue.antiqua.graphics.{Tile, TileRenderer, Animation}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.world.World

object Capsule {
  def create = {
    val fuse = Animation.create(Vector(
      (1, CP437.*.mkTile(Color.Black, Color.Yellow)),
      (1, CP437.Θ.mkTile(Color.Black, Color.Red)),
      (1, CP437.☼.mkTile(Color.Black, Color.Orange))

    ))
    val stick = Animation.singleton(
      CP437.║.mkTile(Color.Black, Color.Red.dim(2))
    )
    Capsule(Seq(
      (0, 0, stick),
      (0, -1, fuse)
    ), 0)
  }
}

case class Capsule private (a:Seq[(Int,Int,Animation)], t:Int){
  def update = copy(a=a.smap {_.update}, t=t+1)

  def isDone = t > 120

  def getDim = 1 + scala.math.sin(t/2.0)/4.0

  private def drawAura(p:Int, q:Int)(tr:TileRenderer):TileRenderer = {
    val bound = (getDim*4).toInt
    val dist = (bound*bound).sqrt
    val draws = for (i <- (p - bound) to (p + bound); j <- (q - bound) to (q + bound)) yield {
      def f(t:Tile) = t.setFg(t.fgColor.dim(1/getDim))
      if (scala.math.hypot(p - i, q - j) < dist) {
        (i, j, f _)
      } else {
        (i, j, id[Tile] _)
      }
    }
    tr `$$>` draws
  }

  def getExplode(i:Int, j:Int):World=>World = id[World] _

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <## (a |+| (i, j)) <+< drawAura(i, j)
  }
}
