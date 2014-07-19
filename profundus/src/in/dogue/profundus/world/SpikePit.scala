package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.data.{Array2d, Direction}
import in.dogue.antiqua.{data, Antiqua}
import Antiqua._
import com.deweyvm.gleany.data.Recti

case class SpikePit(x:Int, y:Int, width:Int, height:Int) {
  def placeShaft(cols:Int, rows:Int, yy:Int, scheme:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    val shaftStart = yy + r.nextInt(rows/4)
    val newTiles = terrain.map { case (i, j, t) =>
      val jSpan = j >= y && j <= y + height
      val inShaft = x > i && x < i + width && jSpan
      val tt = if ((i == x || i  + width - 1 == x) && j + yy > shaftStart && jSpan) {
        scheme.makeShaft(r).some
      } else if (inShaft && j == y + height) {
        scheme.makeRock(r).some
      } else if (inShaft && j == (y + height) - 1) {
        scheme.makeSpike(Direction.Up)(r).some
      } else if (inShaft) {
        scheme.makeEmpty(r).some
      } else {
        None
      }
      tt.map(WorldTile.apply).getOrElse(t)
    }
    (newTiles, Seq())
  }
  def toFeature(cols:Int, rows:Int):Feature = {
    val rect = Recti(x, y, width, height)
    val f = placeShaft _
    Feature(rect, f)
  }
}
