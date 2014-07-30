package in.dogue.profundus.world.features

import in.dogue.antiqua.graphics.Tile
import in.dogue.antiqua.data.{Array2d, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.world._
import scala.util.Random
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.utils.TmxMap
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.world.WorldTile
import in.dogue.profundus.world.Feature

object Lair {
  val tmx = new TmxMap("lair", "tiles")
}
class Lair {
  import Lair._
  def create(cols:Int, rows:Int, y:Int, ts:TerrainScheme, tiles:Array2d[WorldTile], r:Random) = {
    val tf = ts.toFactory(r)
    val (nt, deps) = tiles.map { case (p, t) =>
      if (tmx.get(p) < 1) {
        tf.mkEmpty
      } else {
        tf.mkShaft
      }

    }.unzip
    val newTiles = Terrain.merge(nt, deps)
    newTiles @@ Seq()
  }

  def toFeature(cols:Int, rows:Int):Feature = {
    val rect = Recti(0, 0, cols, rows)
    val f = create _
    Feature.create(rect, f)
  }
}
