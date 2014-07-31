package in.dogue.profundus.world.features

import in.dogue.profundus.world._
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import in.dogue.profundus.world.WorldTile
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.world.dungeon.{DungeonCell, Dungeon}
import in.dogue.antiqua.Antiqua
import Antiqua._

class DungeonFeature(x:Int, y:Int, cols:Int, rows:Int, r:Random) {
  val dCols = cols/DungeonCell.cellSize
  val dRows = (rows*6)/DungeonCell.cellSize
  val dungeon = Dungeon.create(dCols, dRows, 0.5, r)


  def toFeature(cols:Int, rows:Int):Feature = {
    val (mask, msgs) = dungeon.getMask(rows, (x, y))

    val split = MegaFeature.stamp((x, y), cols, rows, mask)
    def genNth(k:Int)(cols:Int, rows:Int, y:Int, ts:TerrainScheme, tiles:Array2d[WorldTile], r:Random) = {
      val tf = ts.toFactory(r)
      val (_, _, myTiles) = split(k)
      val (nt, gen) = tiles.map { case (p, t) =>
        myTiles.getOption(p) match {
          case None => t @@ None
          case Some(b) => b.select(tf.mkEmpty, tf.mkShaft)
        }
      }.unzip
      val newTiles = Terrain.merge(nt, gen)
      newTiles @@ msgs(k)
    }
    val ks = split.keys.toSeq.sorted.reverse.toList
    val (first, rest) = ks match {
      case z :: zs => (z, zs)
      case _ => throw new RuntimeException("Empty dungeon!")
    }
    def getRect(i:Int) = split(i)._2//Recti(x, y, mask.cols, mask.rows)
    def mkFeature(i:Int) = {
      Feature.create(getRect(i), genNth(i))
    }
    rest.foldLeft(mkFeature(first)) { case (nextFeat, k) =>
      mkFeature(k).withFuture(nextFeat)
    }
  }
}
