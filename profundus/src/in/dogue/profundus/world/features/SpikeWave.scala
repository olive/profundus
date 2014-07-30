package in.dogue.profundus.world.features

import in.dogue.profundus.world.{Terrain, Feature, WorldTile, TerrainScheme}
import in.dogue.antiqua.data.{Direction, Array2d}
import scala.util.Random
import in.dogue.antiqua.geometry.{Line, Circle}
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua
import Antiqua._
case class SpikeWave(xy:Cell, width:Int, height:Int, wave:Int => Int) {
  def placeSite(cols:Int, rows:Int, yy:Int, scheme:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    val tf = scheme.toFactory(r)
    val x = xy.x
    val y = xy.y
    val (nt, deps) = terrain.map { case ((i, j), t) =>
      val wv = wave(i) + y
      val iRange = i > x && i < x + width
      if (!iRange) {
        t @@ None
      } else if (wv == j - 1) {
        tf.mkRock1
      } else if (wv == j) {
        tf.mkSpike((i,j), Direction.Up)
      } else if (wv == j + height + 1) {
        tf.mkRock1
      } else if (wv == j + height) {
        tf.mkSpike((i,j), Direction.Down)
      } else if (wv > j && wv < j + height) {
        if (r.nextDouble < 0.1 && wv > j + 3 && wv < j + height - 3) {
          tf.mkRock1
        } else {
          tf.mkEmpty
        }
      } else  {
        t @@ None
      }

    }.unzip

    val newTiles = Terrain.merge(nt, deps)

    newTiles @@ Seq()
  }
  def toFeature(cols:Int, rows:Int):Feature = {
    val rect = Recti(xy.x, xy.y, width, height)
    val f = placeSite _
    Feature.create(rect, f)
  }
}
