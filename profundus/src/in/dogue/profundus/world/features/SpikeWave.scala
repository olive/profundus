package in.dogue.profundus.world.features

import in.dogue.profundus.world.{Feature, WorldTile, TerrainScheme}
import in.dogue.antiqua.data.{Direction, Array2d}
import scala.util.Random
import in.dogue.antiqua.geometry.{Line, Circle}
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua
import Antiqua._
case class SpikeWave(xy:Cell, width:Int, height:Int, wave:Int => Int) {
  def placeSite(cols:Int, rows:Int, yy:Int, scheme:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    val x = xy.x
    val y = xy.y
    val newTiles = terrain.map { case ((i, j), t) =>
      val wv = wave(i) + y
      val iRange = i > x && i < x + width
      if (!iRange) {
        t
      } else if (wv == j - 1) {
        WorldTile(scheme.makeRock(r))
      } else if (wv == j) {
        WorldTile(scheme.makeSpike(Direction.Up)(r))
      } else if (wv == j + height + 1) {
        WorldTile(scheme.makeRock(r))
      } else if (wv == j + height) {
        WorldTile(scheme.makeSpike(Direction.Down)(r))
      } else if (wv > j && wv < j + height) {
        if (r.nextDouble < 0.1 && wv > j + 3 && wv < j + height - 3) {
          WorldTile(scheme.makeRock(r))
        } else {
          WorldTile(scheme.makeEmpty(r))
        }
      } else  {
        t
      }

    }

    newTiles @@ Seq()
  }
  def toFeature(cols:Int, rows:Int):Feature = {
    val rect = Recti(xy.x, xy.y, width, height)
    val f = placeSite _
    Feature(rect, f)
  }
}
