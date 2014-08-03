package in.dogue.profundus.world.features

import in.dogue.profundus.world._
import in.dogue.antiqua.data.Array2d
import scala.util.Random
import in.dogue.profundus.world.WorldTile
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.world.dungeon._
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.world.WorldTile
import scala.Some
import com.deweyvm.gleany.graphics.Color

class DungeonFeature(x:Int, y:Int, cols:Int, rows:Int, r:Random) {
  val dCols = 4//cols/DungeonCell.cellSize
  val dRows = 6//(rows*6)/DungeonCell.cellSize
  val (dungeon, top, bottom) = Dungeon.create(dCols, dRows, 0.5, r)

  def toFeature(cols:Int, rows:Int):Feature = {
    val (dmask, cone, msgs) = dungeon.getMask(rows, (x, y), top, bottom)
    val mask = cone.getMask(dungeon.cols, dungeon.rows, dmask)
    mask.render("feature.png") {
      case Wall => Color.Black
      case Interior => Color.White
      case Exterior => Color.Black
      case Blocked => Color.Black
      case ConeSpace => Color.White
    }
    val split = MegaFeature.stamp((0, 0), cols, rows, mask)
    def genNth(k:Int)(cols:Int, rows:Int, y:Int, ts:TerrainScheme, tiles:Array2d[WorldTile], r:Random) = {
      val tf = ts.toFactory(r)
      val (rect, myTiles) = split(k)
      val pos = rect.x @@ rect.y
      val (nt, gen) = tiles.map { case (p, t) =>
        if (p.x < 2 || p.x > cols - 3) {
          tf.mkShaft
        } else {
          myTiles.getOption(p |- pos.x) match {
            case None => t @@ None
            case Some(b) => b match {
              case Wall => tf.mkShaft
              case Exterior => tf.mkShaft//t @@ None
              case Interior => tf.mkEmpty
              case Blocked => tf.mkShaft
              case ConeSpace => t @@ None
            }
          }
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
    def getRect(i:Int) = split(i)._1 + Recti(0, y, 0, 0)
    def mkFeature(i:Int) = {
      Feature.create(true, getRect(i), genNth(i))
    }
    rest.foldLeft(mkFeature(first)) { case (nextFeat, k) =>
      mkFeature(k).withFuture(nextFeat)
    }
  }
}
