package in.dogue.profundus.world.features

import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.data.Array2d
import in.dogue.antiqua.data
import in.dogue.profundus.world.{WorldTileFactory, TerrainScheme, GlobalMessage, WorldTile}
import scala.util.Random
import in.dogue.profundus.world.features.MegaFeature.{FeatureInput, FeatureOutput}

object MegaFeature {

  private def cut[T](rect:Recti, p1:Cell, tiles:Array2d[T]):Option[(Cell, Recti, Array2d[T])] = {
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

  def stamp[T](pos:Cell, cols:Int, rows:Int, tiles:Array2d[T]):Map[Int,(Cell, Recti, Array2d[T])] =  {
    def getRect(i:Int) = {
      Recti(0, rows*i, cols, rows)
    }
    val start = pos.y / rows
    val end = math.ceil((pos.y + tiles.rows) / rows.toFloat).toInt
    println("Start (%d) End (%d)".format (start, end))
    val rects = (start until end) map { (i:Int) =>
      val p = (pos.x, pos.y + rows * (start - i))
      val r = getRect(i)
      val c = cut(r, p, tiles)
      c.map { case (cell, rect, array) => (i, (cell, rect, array))}
    }
    rects.flatten.toMap
  }


  def test() = {
    val cols = 32*4
    val rows = 48


    val myTiles = Array2d.tabulate(10, 199) { case p => p }
    printRes(stamp((5, 83), cols, rows, myTiles))
  }


  def printRes[T](m:Map[Int, (Cell, Recti, Array2d[T])]) = {
    for ((i, (p, _, arr)) <- m) {
      println("At index %d, (%s) dim(%d,%d)".format(i, p, arr.cols, arr.rows))
    }
  }
  type FeatureOutput = (Array2d[WorldTile], Seq[GlobalMessage])
  type FeatureInput = (Int,Int,Int,WorldTileFactory, Random)
}
case class MegaFeature(f  : FeatureInput
                         => (Recti, Map[Int, FeatureInput => FeatureOutput])) {

  def transform(cols:Int, rows:Int, y:Int, ts:TerrainScheme, r:Random) = {
    ??? //f(cols, rows, y, ts, r)
  }
}

class MegaShaft {
  def create(cols:Int, rows:Int, y:Int, tf:WorldTileFactory, r:Random) = {


  }
}

