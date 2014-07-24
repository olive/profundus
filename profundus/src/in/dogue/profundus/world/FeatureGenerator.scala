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
import in.dogue.antiqua.geometry.Circle

object FeatureGenerator {

  private def makeSpikes(num:Int, cols:Int, rows:Int)(ts:TerrainScheme,  r:Random) = {
    val recti = Recti(cols/2,rows/2,0,0)
    Seq(Feature(recti, spikes(num)))

  }
  private def spikes(num:Int)(cols:Int, rows:Int, y:Int, ts:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    var done = 0
    import Profundus._
    def get(ij:Cell):Boolean = terrain.getOption(ij
    ).exists{_.isWalkable}
    val tiles = r.shuffle(terrain.flatten)
    val tmap = tiles.map { case (ij, t) =>
      val isDone = done > num
      val t = terrain.get(ij)
      val down = get(ij +| 1)
      val up = get(ij -| 1)
      val (next, isDown) = if (!isDone && r.nextDouble > 0.9) {
        if (down && !up && t.isWalkable) {
          done += 1
          (WorldTile(ts.makeSpike(Direction.Down)(r)), true)
        } else if (!isDone && up && !down && t.isWalkable) {
          done += 1
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
      (ij, (next, ems))
    }.toMap

    val ems = (for ((_, em) <- tmap.values) yield {
      em
    }).flatten.toVector

    val newTiles = Array2d.tabulate(cols, rows) { case c =>
      tmap(c)._1
    }

    (newTiles, ems.gss)

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

  def simple(cols:Int, rows:Int, y:Int, ts:TerrainScheme, r:Random, u:Unit) = {
    val spikeWaves = makeSpikeWaves(1, cols, rows)(ts, r)
    val pits = makePits(3, cols, rows)(ts, r)
    val spikes = makeSpikes(1000, cols, rows)(ts, r)
    val shafts = makeShafts(2, cols, rows)(ts, r)
    val camps = makeCampsites(1, cols, rows)(ts, r)
    val cavern = makeCaverns(1, cols, rows)(ts, r)
    val shop = mkShop(cols, rows, y, ts, r, u)
    val all = Vector(spikeWaves, cavern, pits, shafts, camps)
    val (a, b, c) = ts.color.ways3(all)
    a ++ b ++ c ++ spikes ++ shop
  }

  val dummy = FeatureGenerator[Unit](simple)


  private def mkSky(cols:Int, rows:Int, y:Int, ts:TerrainScheme, r:Random, args:Unit) = {
    Seq(CaveMouth.skyFeature(cols, rows))
  }
  val sky = FeatureGenerator(mkSky)

  //(Vector[Seq[Cell]], Circle)
  private def mkSurface(cols:Int, rows:Int, y:Int, ts:TerrainScheme, r:Random, args:(Direction, Vector[Seq[Cell]], Circle)) = {
    Seq(Feature(Recti(0, 0, cols, rows), CaveMouth.createMouth(args._1, args._2, args._3)))
  }

  val surface = FeatureGenerator(mkSurface)


  private def mkShop(cols:Int, rows:Int, y:Int, ts:TerrainScheme, r:Random, args:Unit) = {
    (0 until 10).map { case i =>
      val x = r.nextInt(cols - 16)
      val y = r.nextInt(rows - 14)
      new Shop(x, y).toFeature(cols, rows)
    }

  }


  private def mkLair(cols:Int, rows:Int, y:Int, ts:TerrainScheme, r:Random, args:Unit) = {
    Seq(new Lair().toFeature(cols, rows))
  }

  val lair = FeatureGenerator(mkLair)

  private def mkAbyss(cols:Int, rows:Int, y:Int, ts:TerrainScheme, r:Random, args:Unit) = {
    Seq(new Abyss().toFeature(cols, rows))
  }

  val abyss = FeatureGenerator(mkAbyss)
}

case class FeatureGenerator[T](private val f:(Int, Int, Int, TerrainScheme, Random, T) => Seq[Feature]) {
  def assemble(cols:Int, rows:Int, y:Int, ts:TerrainScheme, r:Random, t:T) = {
    val screenRect = Recti(2, 2, cols-4, rows-4)
    val feats = f(cols, rows, y, ts, r, t)
    val result = ArrayBuffer[Feature]()
    for (feat <- feats) {
      var foundCollision = false
      for (r <- result) {
        if (feat.rect.area > 0 && (r.intersects(feat) || screenRect.containsRect(feat.rect))) {
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
