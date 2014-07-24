package in.dogue.profundus.world.features

import in.dogue.profundus.world.{Feature, WorldTile, TerrainScheme}
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua
import Antiqua._

case class Mineshaft(x:Int, y:Int, width:Int, height:Int) {
  val rect = Recti(x, y, width, height)
  def create(cols:Int, rows:Int, y:Int, ts:TerrainScheme, tiles:Array2d[WorldTile], r:Random) = {
    val newTiles = tiles.map { case ((i, j), t) =>
      val inRange = rect.contains((i, j))
      if (inRange && (i == x + 3  && j % 6 == 0) || (i == x + width - 4 && (j + 3) % 6 == 0)) {
        WorldTile(ts.makeRock(r))
      } else if (inRange && i == x + 2|| i == x + width - 3) {
        WorldTile(ts.makeRock2(r))
      } else if (inRange && (i < x + 2 || i > x + width - 3 )) {
        val d = r.nextDouble
        if (d < 0.03) {
          WorldTile(ts.makeMineral(r))
        } else if (d < 0.33) {
          WorldTile(ts.makeRock(r))
        } else if (d < 0.66){
          WorldTile(ts.makeDirt(r))
        } else {
          WorldTile(ts.makeClay(r))
        }
      } else if (inRange) {
        WorldTile(ts.makeEmpty(r))
      } else {
        t
      }

    }

    newTiles @@ Seq()
  }

  def toFeature(cols:Int, rows:Int):Feature = {
    val rect = Recti(x, y, width, height)
    val f = create _
    Feature(rect, f)
  }
}
