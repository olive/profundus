package in.dogue.profundus.world.features

import in.dogue.antiqua.utils.TmxMap
import in.dogue.profundus.world.{Feature, WorldTile, TerrainScheme}
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua
import Antiqua._

object Abyss {
  val tmx = new TmxMap("abyss", "tiles")
}
class Abyss {
  import Abyss._
  def create(cols:Int, rows:Int, y:Int, ts:TerrainScheme, tiles:Array2d[WorldTile], r:Random) = {
    val newTiles = tiles.map { case (p, t) =>
      if (tmx.get(p) < 1) {
        WorldTile(ts.makeEmpty(r))
      } else {
        WorldTile(ts.makeShaft(r))
      }

    }

    newTiles @@ Seq()
  }

  def toFeature(cols:Int, rows:Int):Feature = {
    val rect = Recti(0, 0, cols, rows)
    val f = create _
    Feature(rect, f)
  }
}
