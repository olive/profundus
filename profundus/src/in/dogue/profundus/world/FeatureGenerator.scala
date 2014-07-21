package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.data.{Array2d, Direction}
import in.dogue.antiqua.Antiqua._
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.world.features._
import scala.collection.mutable.ArrayBuffer
import in.dogue.profundus.world.features.Mineshaft
import in.dogue.profundus.world.features.SpikePit
import in.dogue.profundus.world.features.Campsite
import in.dogue.profundus.world.features.Cavern
import in.dogue.profundus.particles.{Emitter, DropEmitter, WaterDrop}
import in.dogue.profundus.Profundus

object FeatureGenerator {

  private def makeSpikes(num:Int, cols:Int, rows:Int)(ts:TerrainScheme,  r:Random) = {
    (0 until num) map { i =>
      val x = r.nextInt(cols)
      val y = r.nextInt(rows)
      val recti = Recti(x, y, 1, 1)

      Feature(recti, spike((x, y)))
    }
  }
  private def spike(ij:Cell)(cols:Int, rows:Int, y:Int, ts:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    import Profundus._
    def get(ij:Cell):Boolean = terrain.getOption(ij
    ).exists{_.isWalkable}
    val t = terrain.get(ij)
    val down = get(ij +| 1)
    val up = get(ij -| 1)
    val (next, isDown) = if (r.nextDouble > 0.9) {
      if (down && !up && t.isWalkable) {
        (WorldTile(ts.makeSpike(Direction.Down)(r)), true)
      } else if (up && !down && t.isWalkable) {
        (WorldTile(ts.makeSpike(Direction.Up)(r)), false)
      } else {
        (t, false)
      }

    } else {
      (t, false)
    }
    val ems = if (r.nextDouble > 0.5 && isDown) {
      Seq(DropEmitter.create(ij +| y --> Direction.Down, 60 + r.nextInt(60), math.abs(r.nextInt(10000))).toEmitter)
    } else {
      Seq()
    }
    (terrain.updated(ij, next), Seq(), Seq(ems.gs))

  }

  private def makePits(num:Int, cols:Int, rows:Int)(ts:TerrainScheme, r:Random) = {
    val width = 13
    val height = 10
    (0 until num) map { case i =>
      val xx = r.nextInt(cols - width)
      val yy = r.nextInt(rows - height)
      SpikePit(xx, yy, width, height).toFeature(cols, rows)
    }

  }

  private def makeShafts(num:Int, cols:Int, rows:Int)(ts:TerrainScheme, r:Random) = {
    val width = 9
    val height = 32
    (0 until num) map { case i =>
      val xx = r.nextInt(cols - width)
      val yy = r.nextInt(rows - height)
      Mineshaft(xx, yy, width, height).toFeature(cols, rows)
    }
  }

  private def makeCampsites(num:Int, cols:Int, rows:Int)(ts:TerrainScheme, r:Random) = {
    val radius = 7
    (0 until num) map { case i =>
      val xx = r.nextInt(cols - (radius*2))
      val yy = r.nextInt(rows - (radius*2))
      Campsite((xx, yy), radius).toFeature(cols, rows)
    }
  }

  private def makeCaverns(num:Int, cols:Int, rows:Int)(ts:TerrainScheme, r:Random) = {
    val radius = 7
    (0 until num) map { case i =>
      val xx = r.nextInt(cols - (radius*2))
      val yy = r.nextInt(rows - (radius*2))
      Cavern((xx, yy), radius).toFeature(cols, rows)
    }
  }

  private def makeSpikeWaves(num:Int, cols:Int, rows:Int)(ts:TerrainScheme, r:Random) = {
    val height = 10
    val width = 55
    (0 until num) map { case i =>
      val xx = r.nextInt(cols - width)
      val yy = r.nextInt(rows - height)

      def wave(t:Int) = {
        yy + ((height/2) * math.sin(t/10f)).toInt
      }
      SpikeWave((xx, yy), width, height, wave).toFeature(cols, rows)
    }
  }

  def simple(cols:Int, rows:Int, y:Int, ts:TerrainScheme, r:Random) = {
    val spikeWaves = makeSpikeWaves(1, cols, rows)(ts, r)
    val pits = makePits(3, cols, rows)(ts, r)
    val spikes = makeSpikes(1000, cols, rows)(ts, r)
    val shafts = makeShafts(2, cols, rows)(ts, r)
    val camps = makeCampsites(1, cols, rows)(ts, r)
    val cavern = makeCaverns(1, cols, rows)(ts, r)
    spikeWaves ++ cavern ++ pits ++ shafts ++ camps ++ spikes
  }

  val dummy = FeatureGenerator(simple)

}

case class FeatureGenerator(private val f:(Int, Int, Int, TerrainScheme, Random) => Seq[Feature]) {
  def assemble(cols:Int, rows:Int, y:Int, ts:TerrainScheme, r:Random) = {
    val feats = f(cols, rows, y, ts, r)
    val result = ArrayBuffer[Feature]()
    for (feat <- feats) {
      var foundCollision = false
      for (r <- result) {
        if (r.intersects(feat)) {
          foundCollision = true
        }
      }
      if (!foundCollision) {
        result += feat
      }
      ()
    }
    result.toSeq
  }
}
