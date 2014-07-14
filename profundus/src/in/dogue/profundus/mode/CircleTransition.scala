package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color

object CircleTransition {
  def create(cols:Int, rows:Int, old:Mode[_], `new`:Mode[_]) = {
    CircleTransition(cols, rows, old, `new`, 0, 120)
  }
}

case class CircleTransition private (cols:Int, rows:Int, old:Mode[_], `new`:Mode[_], t:Int, max:Int) {
  def update = {
    if (t > max) {
      `new`
    } else {
      copy(t=t+1).toMode
    }
  }

  private def drawCover(tr:TileRenderer):TileRenderer = {
    val half = max/2
    val col2 = cols/2.sqrt
    def f(tile:Tile) = tile.setBg(Color.Black).setFg(Color.Black)
    val tt = if (t < half) {
      half - t
    } else {
      t - half
    }
    val r = ((tt/half.toFloat)*col2).toInt


    val draws = for (i <- 0 until cols; j <- 0 until rows) yield {
      val hyp = scala.math.hypot(i - cols/2, j - rows/2)
      if (hyp >= r) {
        (i, j, f _).some
      } else {
        None
      }
    }
    tr `$$>` draws.flatten

  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< (if (t < max/2) {
      old.draw
    } else {
      `new`.draw
    }) <+< drawCover
  }

  def toMode:Mode[CircleTransition] = Mode(_.update, _.draw, this)
}
