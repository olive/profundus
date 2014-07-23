package in.dogue.profundus.world.features

import in.dogue.antiqua.graphics.Tile
import in.dogue.antiqua.data.{Array2d, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.world.{TileType, Feature, WorldTile, TerrainScheme}
import scala.util.Random
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.utils.TmxMap


object Lair {
  val tmx = new TmxMap("lair", "tiles")
}
class Lair {
  import Lair._
  def create(cols:Int, rows:Int, y:Int, ts:TerrainScheme, tiles:Array2d[WorldTile], r:Random) = {
    val newTiles = tiles.map { case (p, t) =>
      if (tmx.get(p) < 1) {
        WorldTile(ts.makeEmpty(r))
      } else {
        WorldTile(ts.makeDirt(r))
      }

    }

    (newTiles, Seq(), Seq())
  }

  def toFeature(cols:Int, rows:Int):Feature = {
    val rect = Recti(0, 0, cols, rows)
    val f = create _
    Feature(rect, f)
  }
}
