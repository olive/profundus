package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.data.Direction
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.data.Recti

case class SpikePit(x:Int, y:Int, width:Int, height:Int) {
  def placeShaft(cols:Int, rows:Int, scheme:TerrainScheme)(terrain:Terrain)(r:Random):Terrain = {
    val shaftStart = y + r.nextInt(rows/4)
    val newTiles = terrain.tiles.map { case (i, j, t) =>
      val inShaft = x > i && x < i + width
      val tt = if ((i == x || i  + width - 1== x) && j > shaftStart) {
        scheme.makeShaft(r).some
      } else if (inShaft && j == y + height) {
        scheme.makeRock(r).some
      } else if (inShaft && j  == y+ height - 1) {
        scheme.makeSpike(Direction.Up)(r).some
      } else if (inShaft) {
        scheme.makeEmpty(r).some
      } else {
        None
      }
      tt.map(WorldTile.apply).getOrElse(t)
    }
    terrain.copy(tiles=newTiles)
  }
  def toFeature(cols:Int, rows:Int, scheme:TerrainScheme):Feature = {
    val rect = Recti(x, y, width, height)
    val f = placeShaft(cols, rows, scheme) _
    Feature(rect, f)
  }
}
