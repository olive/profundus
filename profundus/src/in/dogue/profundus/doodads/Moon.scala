package in.dogue.profundus.doodads

import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.Antiqua
import Antiqua._

object Moon {
  def create(x:Int, y:Int, r:Int) = {
    val draws = for (i <- 0 to r*2; j <- 0 to r*2) yield {
      if (scala.math.hypot(r - i, r - j) < r) {
        (i, j,  CP437.█.mkTile(Color.Black, Color.White)).some
      } else {
        None
      }

    }
    Moon(x, y, r, draws.flatten, 0)
  }
}

case class Moon private (i:Int, j:Int, r:Int, tg:TileGroup, t:Int) {

  def update = copy(t=t+1)

  def drawBright(tr:TileRenderer):TileRenderer = {
    val max = r*4
    val draws = for (h <- 0 to r*4; k <- 0 to r*4) yield {
      import scala.math.{hypot, sin, abs}
      val hyp = hypot(max/2 - h, max/2 - k)
      def f(tile:Tile): Tile = tile.setBg(tile.bgColor.dim((1 - 1/hyp)*(1 - 1/hyp) - abs(sin(t/60f)/10f)))
      if (hyp >= r && hyp < max/2) {
        (h, k, f _).some
      } else {
        None
      }

    }
    tr `$$>` (draws.flatten |+| (i-max/4, j-max/4))
  }



  def draw(tr:TileRenderer):TileRenderer = {
    tr <++ (tg |+| (i, j)) <+< drawBright
  }

  def toDoodad:Doodad[Moon] = Doodad(_.update, _.draw, this)
}
