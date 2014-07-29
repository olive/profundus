package in.dogue.profundus.world.features

import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.data.Array2d
import in.dogue.antiqua.data
import in.dogue.profundus.world.{TerrainScheme, Feature, GlobalMessage, WorldTile}
import scala.util.Random
import in.dogue.profundus.world.features.MegaFeature.FeatureOutput

object MegaFeature {
  def cut[T](rect:Recti, p1:Cell, tiles:Array2d[T]):Option[(Cell, Recti, Array2d[T])] = {
    val tRect = Recti(p1.x, p1.y, tiles.cols, tiles.rows)
    val overlap = rect.getOverlap(tRect)
    overlap.map { over => cutRects(over, p1, tiles)}
  }


  def cutRects[T](overlap:Recti, p1:Cell, tiles:Array2d[T]):(Cell, Recti, Array2d[T]) = {
    val p3 = overlap.x @@ overlap.y
    val w = overlap.width
    val h = overlap.height
    val offset = p3 |-| p1
    val newTiles = Array2d.tabulate(w, h) { case p =>
      tiles.get(p |+| offset)
    }
    (p3, overlap, newTiles)
  }

  /**
   * @param pos upper left of the position of tiles
   * @param getRect the current terrain rect at index i
   * @param tiles the tiles to be stamped
   */
  def stamp[T](pos:Cell, height:Int, getRect:Int=>Recti, tiles:data.Array2d[T]):Map[Int,(Cell, Recti, Array2d[T])] =  {
    val start = pos.y / height
    val end = (pos.y + tiles.rows) / height
    val rects = (start until end) map { (i:Int) =>
      val p = (pos.x, pos.y + height * (start - i))
      val r = getRect(i)
      val c = cut(r, p, tiles)
      c.map { case (cell, rect, array) => (i, (cell, rect, array))}
    }
    rects.flatten.toMap
  }


  def test() = {
    val cols = 32*4
    val rows = 48
    def getRect(i:Int) = {
      Recti(0, rows*i, cols, rows)
    }

    val myTiles = Array2d.tabulate(10, 199) { case p => p }
    printRes(stamp((5, 83), rows, getRect, myTiles))
  }


  def printRes[T](m:Map[Int, (Cell, Recti, Array2d[T])]) = {
    for ((i, (p, _, arr)) <- m) {
      println("At index %d, (%s) dim(%d,%d)".format(i, p, arr.cols, arr.rows))
    }
  }
  type FeatureOutput = (Array2d[WorldTile], Seq[GlobalMessage])
}
case class MegaFeature(f  : (Int,Int,Int,TerrainScheme, Random)
                         => (Recti, FeatureOutput, Map[Int, FeatureOutput])) {

  type MegaFeatureCreate = (Int,Int,Int,TerrainScheme, Random) => (Array2d[WorldTile], Map[Int, Seq[GlobalMessage]])

  def transform(cols:Int, rows:Int, y:Int, ts:TerrainScheme, r:Random) = {
    f(cols, rows, y, ts, r)
  }



}
