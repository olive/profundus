package in.dogue.profundus.world

import in.dogue.antiqua.graphics.{TileRenderer}
import in.dogue.antiqua.data.{CP437, Array2d}
import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Implicits
import Implicits._

object Terrain {
  def create(y:Int, cols:Int, rows:Int, r:Random) = {
    val tiles = Array2d.tabulate(cols, rows) { case (i, j) =>
      val bg = Color.Brown.dim(3 + r.nextDouble)
      val fg = Color.Tan.dim(1 + r.nextDouble)
      val bgCode = Vector(CP437.`.`, CP437.`,`, CP437.`'`, CP437.`"`).randomR(r)
      val solid = CP437.█.mkTile(bg, fg)
      val empty = bgCode.mkTile(bg, fg)
      val state = (r.nextDouble > 0.6).select(Filled, Empty)
      WorldTile(solid, empty, state)
    }
    Terrain(y, tiles)
  }

}

case class Terrain private (y:Int, tiles:Array2d[WorldTile]) {
  def isSolid(s:(Int,Int)):Boolean = {
    val t = (tiles.getOption _).tupled(s)
    !t.exists{_.state == Empty}
  }

  def break(ij:(Int,Int)) = {
    copy(tiles=tiles.update(ij.x, ij.y, _.copy(state=Empty)))
  }

  def draw(tr:TileRenderer):TileRenderer = {
    //tr.<+++<(tiles, (w:WorldTile) => w.getTile)
    tr <++ tiles.flatten.map { case (i, j, w)  => (i, j+y, w.getTile)}
  }
}
