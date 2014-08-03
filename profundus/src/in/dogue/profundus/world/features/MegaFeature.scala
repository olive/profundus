package in.dogue.profundus.world.features

import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.data.Array2d

object MegaFeature {

  private def cut[T](rect:Recti, p1:Cell, tiles:Array2d[T]):Option[(Recti, Array2d[T])] = {
    val tRect = Recti(p1.x, p1.y, tiles.cols, tiles.rows)
    val overlap = rect.getOverlap(tRect)
    overlap.map { over => cutRects(over, p1, tiles)}
  }


  def cutRects[T](overlap:Recti, p1:Cell, tiles:Array2d[T]):(Recti, Array2d[T]) = {
    val p3 = overlap.x @@ overlap.y
    val w = overlap.width
    val h = overlap.height
    val offset = p3 |-| p1
    val newTiles = Array2d.tabulate(w, h) { case p =>
      tiles.get(p |+| offset)
    }
    (overlap, newTiles)
  }

  def stamp[T](pos:Cell, cols:Int, rows:Int, tiles:Array2d[T]):Map[Int,(Recti, Array2d[T])] =  {
    def getRect(i:Int) = {
      Recti(0, rows*i, cols, rows)
    }
    val start = pos.y / rows
    val end = math.ceil((pos.y + tiles.rows) / rows.toFloat).toInt
    val rects = (start until end) map { (i:Int) =>
      val r = getRect(i)
      val c = cut(r, pos, tiles)
      c.map { case (rect, array) => (i, (rect, array))}
    }
    rects.flatten.toMap
  }

}


