package in.dogue.profundus.doodads

import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.lighting.LightSource
import com.deweyvm.gleany.data.Recti

object Moon {
  def create(cols:Int, rows:Int, xy:Cell, r:Int) = {
    val draws = for (i <- 0 to r*2; j <- 0 to r*2) yield {
      if (scala.math.hypot(r - i, r - j) < r) {
        ((i, j),  CP437.█.mkTile(Color.Black, Color.White)).some
      } else {
        None
      }

    }
    val light = LightSource.createRect((0, 0), cols, rows, 0.5)
    Moon(xy, r, light, draws.flatten, 0)
  }
}

case class Moon private (ij:Cell, r:Int, light:LightSource, tg:TileGroup, t:Int) {
  final val i = ij.x
  final val j = ij.y
  def update = copy(t=t+1)

  def getLight:Option[LightSource] = light.some
  def getPos = ij
  def drawBright(tr:TileRenderer):TileRenderer = {
    val max = r*4
    val draws = for (h <- 0 to r*4; k <- 0 to r*4) yield {
      import scala.math.{hypot, sin, abs}
      val hyp = hypot(max/2 - h, max/2 - k)
      def f(tile:Tile): Tile = tile.setBg(tile.bgColor.dim((1 - 1/hyp)*(1 - 1/hyp) - abs(sin(t/60f)/10f)))
      if (hyp >= r && hyp < max/2) {
        ((h, k), f _).some
      } else {
        None
      }

    }
    tr `$$>` (draws.flatten |++| (ij |-| ((max/4, max/4))))
  }



  def draw(tr:TileRenderer):TileRenderer = {
    tr <++ (tg |++| ij) <+< drawBright
  }

  def toDoodad:Doodad = Doodad[Moon](_.update, _.draw, _.getLight, _.getPos, this)
}
