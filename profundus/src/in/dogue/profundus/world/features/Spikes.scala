package in.dogue.profundus.world.features

import in.dogue.profundus.world._
import scala.util.Random
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.data.{Direction, Array2d}
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.particles.DropEmitter
import in.dogue.profundus.world.WorldTile
import in.dogue.profundus.world.Feature

class Spikes(num:Int) {


  def toFeature(cols:Int, rows:Int) = {
    val recti = Recti(cols/2,rows/2,0,0)
    Feature(recti, spikes(num))
  }

  private def spikes(num:Int)(cols:Int, rows:Int, y:Int, ts:TerrainScheme, terrain:Array2d[WorldTile], r:Random) = {
    var done = 0
    val tf = ts.toFactory(r)
    import Profundus._
    def get(ij:Cell):Boolean = terrain.getOption(ij
    ).exists{_.isWalkable}
    val tiles = r.shuffle(terrain.flatten)
    val tmap = tiles.map { case (ij, t) =>
      val isDone = done > num
      val t = terrain.get(ij)
      val isTop = ij.x == 0
      val down = get(ij +| 1)
      val up = get(ij -| 1)
      val (next, dep, isDown) = if (!isDone && r.nextDouble > 0.9) {
        if (!isTop && down && !up && t.isWalkable) {
          done += 1
          tf.mkSpike(ij, Direction.Down) @@ true
        } else if (!isDone && up && !down && t.isWalkable) {
          done += 1
          tf.mkSpike(ij, Direction.Up) @@ false
        } else {
          t @@ None @@ false
        }

      } else {
        t @@ None @@ false
      }
      val ems = if (r.nextDouble > 0.5 && isDown) {
        Seq(DropEmitter.create(ij +| y --> Direction.Down, 60 + r.nextInt(60), math.abs(r.nextInt(10000))).toEmitter)
      } else {
        Seq()
      }
      (ij, (next, dep, ems))
    }.toMap

    val ems = (for ((_, _, em) <- tmap.values) yield {
      em
    }).flatten.toVector

    val (nt, deps) = Array2d.tabulate(cols, rows) { case c =>
      val p = tmap(c)
      (p._1, p._2)
    }.unzip


    val newTiles = Terrain.merge(nt, deps)
    (newTiles, ems.gms, Seq())

  }
}
