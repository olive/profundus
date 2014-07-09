package in.dogue.antiqua.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.Code
import in.dogue.antiqua.Implicits._

object Border {
  def standard(bg:Color, fg:Color) = Border(Code.║, Code.═, Code.╔, Code.╗, Code.╚, Code.╝)(bg, fg) _
}


case class Border(
    v:Code, h:Code, ul:Code, ur:Code, ll:Code, lr:Code
  )(
    bgColor:Color, fgColor:Color
  )(
    val cols:Int, val rows:Int) {
  private def mkTile(c:Code) = Tile(c, bgColor, fgColor)
  val edges:Seq[(Int,Int,Tile)] = {
    val vert = mkTile(v)
    val horiz = mkTile(h)
    val upLeft = Seq((0,0, mkTile(ul)))
    val top = for (i <- 1 until cols - 1) yield (i, 0, horiz)
    val upRight = Seq((cols - 1, 0, mkTile(ur)))
    val right = for (j <- 1 until rows - 1) yield (cols - 1, j, vert)
    val downRight = Seq((cols - 1, rows - 1, mkTile(lr)))
    val bottom = (for (i <- 1 until cols - 1) yield (i, rows - 1, horiz)).reverse
    val downLeft = Seq((0, rows - 1, mkTile(ll)))
    val left = (for (j <- 1 until rows - 1) yield (0, j, vert)).reverse
    upLeft ++ top ++ upRight ++ right ++ downRight ++ bottom ++ downLeft ++ left
  }

  def draw(p:Int, q:Int)(tr:TileRenderer):TileRenderer = {
    tr <++ edges.map {case (i, j, t) => (i + p, j + q, t)}
  }

  def filterDraw(i:Int, j:Int, f:(Int,Int) => Boolean)(tr:TileRenderer):TileRenderer = {
    tr <++ (edges map { case (ii, jj, t) =>
      if (f(ii, jj)) {
        (i+ii, j+jj, t).some
      } else {
        None
      }

    }).flatten
  }
}
