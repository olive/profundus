package in.dogue.profundus.world.features

import scala.util.Random
import in.dogue.antiqua.data.{Array2d, Direction}
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.world.{Terrain, Feature, WorldTile, TerrainScheme}

case class SpikePit(x:Int, y:Int, width:Int, height:Int) {
  def placeShaft(cols:Int, rows:Int, yy:Int, scheme:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    val tf = scheme.toFactory(r)
    val shaftStart = yy + r.nextInt(rows/4)
    val (nt, gen) = terrain.map { case ((i, j), t) =>
      val jSpan = j >= y && j <= y + height
      val inShaft = x > i && x < i + width && jSpan
      if ((i == x || i  + width - 1 == x) && j + yy > shaftStart && jSpan) {
        tf.mkRock2
      } else if (inShaft && j == y + height) {
        tf.mkRock1
      } else if (inShaft && j == (y + height) - 1) {
        tf.mkSpike((i, j), Direction.Up)
      } else if (inShaft) {
        tf.mkEmpty
      } else {
        t @@ None
      }
    }.unzip
    val newTiles = Terrain.merge(nt, gen)
    newTiles @@ Seq() @@ Seq()
  }
  def toFeature(cols:Int, rows:Int):Feature = {
    val rect = Recti(x, y, width, height)
    val f = placeShaft _
    Feature(rect, f)
  }
}
