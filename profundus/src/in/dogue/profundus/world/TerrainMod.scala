package in.dogue.profundus.world

import in.dogue.antiqua.data.{Direction, Array2d}
import in.dogue.antiqua.Antiqua
import Antiqua._
import scala.util.Random
import in.dogue.profundus.particles.DropEmitter
import in.dogue.profundus.Profundus

object TerrainMod {
  def makeSpikes(num:Int)(wtf:WorldTileFactory, y:Int, arr:Array2d[WorldTile], r:Random):(Array2d[WorldTile], Seq[GlobalMessage]) = {
    val rows = arr.rows
    val cols = arr.cols
    import Profundus._

    var count = 0
    val tiles: Seq[(Cell, WorldTile)] = r.shuffle(arr.flatten)
    def get(ij:Cell):Boolean = arr.getOption(ij).exists{_.isWalkable}

    val tmap = tiles.map { case (ij, t) =>
      val isDone = count > num
      val t = arr.get(ij)
      val isTop = ij.x == 0
      val down = get(ij +| 1)
      val up = get(ij -| 1)
      val (next, dep, isDown) = if (!isDone && r.nextDouble > 0.9) {
        if (!isTop && down && !up && t.isWalkable) {
          count += 1
          wtf.mkSpike(ij, Direction.Down) @@ true
        } else if (!isDone && up && !down && t.isWalkable) {
          count += 1
          wtf.mkSpike(ij, Direction.Up) @@ false
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
    (newTiles, ems.gms)
  }

  def spikes(num:Int) = TerrainMod(makeSpikes(num))
}

case class TerrainMod(private val f:(WorldTileFactory, Int, Array2d[WorldTile], Random) => (Array2d[WorldTile], Seq[GlobalMessage])) {
  def mod(tf:WorldTileFactory, y:Int, arr:Array2d[WorldTile], r:Random)
      :(Array2d[WorldTile], Seq[GlobalMessage])  = {
    f(tf, y, arr, r)
  }
}
