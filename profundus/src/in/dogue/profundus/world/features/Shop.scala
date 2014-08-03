package in.dogue.profundus.world.features

import in.dogue.profundus.world.{Terrain, WorldTile, TerrainScheme, Feature}
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import in.dogue.antiqua.utils.TmxMap
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.{ToolType, Tool, Gouge, Shopkeeper}
import in.dogue.profundus.Profundus
import in.dogue.profundus.entities.pickups.ToolPickup

class Shop(x:Int, y:Int) {
  val tiles = new TmxMap("shop", "tiles")
  def placeSite(cols:Int, rows:Int, yy:Int, scheme:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    import Profundus._
    val tf = scheme.toFactory(r)
    val (nt, gen) = terrain.map { case (p, t) =>
      val g = tiles.getOption(p |-| ((x, y)))
      if (!g.isDefined || g.get > 252) {
        t @@ None
      } else if (g.get == 2) {
        tf.mkShaft
      } else if (g.get > 1) {
        tf.mkRock3
      } else {
        tf.mkEmpty
      }
    }.unzip
    val newTiles = Terrain.merge(nt, gen)
    val skOffset = (6, 10)
    val toolType = ToolType.randomR(r)
    val item = ToolPickup.create((x, y) |+| skOffset -| 5 +| yy, toolType.toTool)
    val sk = Seq(Shopkeeper.create((x, y) |+| skOffset +| yy, item, r)).gms ++ item.seq.gms
    newTiles @@ sk
  }

  def toFeature(yPos:Int, cols:Int, rows:Int):Feature = {
    val rect = Recti(x, y + yPos, tiles.cols, tiles.rows)
    val f = placeSite _
    Feature.create(false, rect, f)
  }
}
