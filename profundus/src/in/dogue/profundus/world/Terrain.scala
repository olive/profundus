package in.dogue.profundus.world

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.data.{CP437, Array2d}
import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Implicits
import Implicits._
import in.dogue.profundus.entities.MineralDrop

object Terrain {
  def create(y:Int, cols:Int, rows:Int, r:Random) = {
    val tiles = Array2d.tabulate(cols, rows) { case (i, j) =>
      val bg = Color.Brown.dim(3 + r.nextDouble)
      val fg = Color.Grey.dim(1 + r.nextDouble)
      val bgCode = Vector(CP437.`.`, CP437.`,`, CP437.`'`, CP437.`"`).randomR(r)
      val solid = CP437.█.mkTile(bg, fg)
      val gemColor = Vector(Color.Purple, Color.Red, Color.Green, Color.Blue).randomR(r)
      val gem = CP437.◘.mkTile(gemColor, fg)
      val empty = bgCode.mkTile(bg, fg)
      val state = Vector(
        (50, Filled),
        (50, Empty),
        (1, Mineral.create(gemColor))
      ).expand.randomR(r)
      WorldTile(solid, empty, gem, state)
    }
    Terrain(y, tiles)
  }

}
/* DONT FORGET TO ADD y TO SPAWN VALUES! */
case class Terrain private (y:Int, tiles:Array2d[WorldTile]) {
  def isSolid(s:(Int,Int)):Boolean = {
    val t = (tiles.getOption _).tupled(s)
    !t.exists{_.state == Empty}
  }

  def break(ij:(Int,Int)):(Terrain, Seq[MineralDrop]) = {
    val t = tiles.get(ij.x, ij.y)
    val (newState: TileState, drops) = t.state match {
      case Filled => (Empty, Seq())
      case Empty => (Empty, Seq())
      case g@Mineral(_,_) => g.hit(ij.x, ij.y + y)
    }
    val newT = copy(tiles=tiles.update(ij.x, ij.y, _.copy(state=newState)))
    (newT, drops)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    //tr.<+++<(tiles, (w:WorldTile) => w.getTile)
    tr <++ tiles.flatten.map { case (i, j, w)  => (i, j+y, w.getTile)}
  }
}
