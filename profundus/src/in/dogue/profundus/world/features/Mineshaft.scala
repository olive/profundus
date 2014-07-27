package in.dogue.profundus.world.features

import in.dogue.profundus.world.{Terrain, Feature, WorldTile, TerrainScheme}
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua
import Antiqua._

case class Mineshaft(x:Int, y:Int, width:Int, height:Int) {
  val rect = Recti(x, y, width, height)
  def create(cols:Int, rows:Int, y:Int, ts:TerrainScheme, tiles:Array2d[WorldTile], r:Random) = {
    val tf = ts.toFactory(r)
    val (nt, gen) = tiles.map { case ((i, j), t) =>
      val inRange = rect.contains((i, j))
      if (inRange && (i == x + 3  && j % 6 == 0) || (i == x + width - 4 && (j + 3) % 6 == 0)) {
        tf.mkRock1
      } else if (inRange && i == x + 2|| i == x + width - 3) {
        tf.mkRock2
      } else if (inRange && (i < x + 2 || i > x + width - 3 )) {
        val d = r.nextDouble
        if (d < 0.03) {
          tf.mkMineral
        } else if (d < 0.33) {
          tf.mkRock1
        } else if (d < 0.66){
          tf.mkDirt
        } else {
          tf.mkClay
        }
      } else if (inRange) {
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
    val f = create _
    Feature(rect, f)
  }
}
