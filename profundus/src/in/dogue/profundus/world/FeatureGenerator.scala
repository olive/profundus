package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.data.{Array2d, Direction}
import in.dogue.antiqua.Antiqua._
import com.deweyvm.gleany.data.Recti

object FeatureGenerator {

  private def makeSpikes(num:Int, cols:Int, rows:Int)(ts:TerrainScheme,  r:Random) = {
    (0 until num) map { i =>
      val x = r.nextInt(cols)
      val y = r.nextInt(rows)
      val recti = Recti(x, y, 1, 1)
      Feature(recti, spike(x, y))
    }
  }
  private def spike(i:Int, j:Int)(cols:Int, rows:Int, y:Int, ts:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    def get(ij:Cell):Boolean = terrain.getOption(ij.x, ij.y).exists{_.isWalkable}
    val t = terrain.get(i, j)
    val p = (i, j)
    val left = get(p |- 1)
    val right = get(p |+ 1)
    val down = get(p +| 1)
    val up = get(p -| 1)
    val next = if (r.nextDouble > 0.9) {
      if (down && !up && t.isWalkable) {
        WorldTile(ts.makeSpike(Direction.Down)(r))
      } else if (up && !down && t.isWalkable) {
        WorldTile(ts.makeSpike(Direction.Up)(r))
      } else {
        t
      }

    } else {
      t
    }

    (terrain.updated(i, j, next), Seq())

  }

  private def makePits(num:Int, cols:Int, rows:Int)(ts:TerrainScheme, r:Random) = {
    val width = 5
    val height = 10
    (0 until num) map { case i =>
      val xx = r.nextInt(cols - width)
      val yy = r.nextInt(rows - height)
      SpikePit(xx, yy, width, height).toFeature(cols, rows)
    }

  }
  def simple(cols:Int, rows:Int, y:Int, ts:TerrainScheme, r:Random) = {
    val pits = makePits(3, cols, rows)(ts, r)
    val spikes = makeSpikes(1000, cols, rows)(ts, r)
    pits ++ spikes
  }

  val dummy = FeatureGenerator(simple)

}

case class FeatureGenerator(f:(Int, Int, Int, TerrainScheme, Random) => Seq[Feature]) {

}
