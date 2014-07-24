package in.dogue.profundus.world.features

import in.dogue.profundus.world.{WorldTile, TerrainScheme, Feature}
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import in.dogue.antiqua.geometry.Circle
import in.dogue.profundus.doodads.Campfire
import in.dogue.antiqua.utils.TmxMap
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.Shopkeeper
import in.dogue.profundus.Profundus

class Shop(x:Int, y:Int) {
  val tiles = new TmxMap("shop", "tiles")
  def placeSite(cols:Int, rows:Int, yy:Int, scheme:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    import Profundus._
    val newTiles = terrain.map { case (p, t) =>
      val g = tiles.getOption(p |-| ((x, y)))
      if (!g.isDefined || g.get > 252) {
        t
      } else if (g.get > 1) {
        WorldTile(scheme.makeRock3(r))
      } else {
        WorldTile(scheme.makeEmpty(r))
      }
    }
    val sk = Seq(Shopkeeper.create((x, y) |+| ((6, 10)) +| yy, r)).gss
    (newTiles, sk)
  }

  def toFeature(cols:Int, rows:Int):Feature = {
    val rect = Recti(x, y, tiles.cols, tiles.rows)
    val f = placeSite _
    Feature(rect, f)
  }
}
