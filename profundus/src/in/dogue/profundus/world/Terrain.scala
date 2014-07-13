package in.dogue.profundus.world

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.data.{CP437, Array2d}
import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Implicits
import Implicits._
import in.dogue.profundus.entities.MineralDrop
import in.dogue.antiqua.procgen.PerlinNoise

object Terrain {
  def create(y:Int, cols:Int, rows:Int, r:Random) = {
    val noise = new PerlinNoise().generate(cols, rows, 0, y, r.nextInt())
    import scala.math.abs
    val tiles = noise.map { case (i, j, d) =>
      val bg = Color.Brown.dim(3 + r.nextDouble)
      val dirtc =  Color.Tan.dim(3 + r.nextDouble)
      val fg = Color.Grey.dim(1 + r.nextDouble)
      val bgCode = Vector(CP437.`.`, CP437.`,`, CP437.`'`, CP437.`"`).randomR(r)
      val rock = CP437.█.mkTile(bg, fg)
      val dirt = CP437.█.mkTile(bg, dirtc)
      val mineralColor = Vector(Color.Purple, Color.Red, Color.Green, Color.Blue).randomR(r)
      val mineral = CP437.◘.mkTile(mineralColor, fg)
      val empty = bgCode.mkTile(bg, fg)
      /*val state = Vector(
        (abs(d*1180).toInt, Filled),
        (abs(d*1180).toInt, Empty),
        (1, Mineral.create(gemColor))
      ).expand.randomR(r)*/
      val state = if (d < -0.2) {
        if (r.nextDouble > 0.99) {
          Mineral.create(mineralColor)
        } else {
          Rock.create

        }
      } else if (d < 0.2) {
        Dirt.create
      } else {
        Empty
      }
      WorldTile(rock, dirt, empty, mineral, state)
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

  def hit(ij:(Int,Int)):(Terrain, Seq[MineralDrop]) = {
    val t = tiles.get(ij.x, ij.y)
    val (newState: TileState, drops) = t.state match {
      case r@Rock(_) => r.hit
      case d@Dirt(_) => d.hit
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
