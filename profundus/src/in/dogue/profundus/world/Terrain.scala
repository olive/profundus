package in.dogue.profundus.world

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.data.{CP437, Array2d}
import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.MineralDrop
import in.dogue.antiqua.procgen.PerlinNoise


object Terrain {
  def createSky(y:Int, cols:Int, rows:Int, r:Random) = {
    val tiles = Array2d.tabulate(cols, rows) { case (i, j) =>
      val bgCode = Vector(CP437.`.`, CP437.`'`, CP437.` `, CP437.` `, CP437.` `).randomR(r)
      val dim = (j+y)/cols.toDouble
      val night = bgCode.mkTile(Color.DarkBlue.dim(1/dim), Color.White)
      val grassFg = Color.DarkGreen.mix(Color.Brown, j/(rows*2).toDouble).dim(1 + r.nextDouble)
      val grassBg = Color.DarkGreen.mix(Color.Brown, j/rows.toDouble).dim(1 + r.nextDouble)
      val grass = CP437.█.mkTile(grassBg, grassFg)
      val behind = CP437.` `.mkTile(grassBg, grassBg)
      val state = if (j + y > 16) {
        Dirt.create(grass, behind)
      } else {
        Empty(night)
      }
      WorldTile(state)
    }
    Terrain(y, tiles)
  }
  def create(y:Int, cols:Int, rows:Int, r:Random) = {
    val noise = new PerlinNoise().generate(cols, rows, 0, y, r.nextInt())
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
      /*import scala.math.abs
      val state = Vector(
        (abs(d*1180).toInt, Filled),
        (abs(d*1180).toInt, Empty),
        (1, Mineral.create(gemColor))
      ).expand.randomR(r)*/
      val state = if (d < -0.4) {
        if (r.nextDouble > 0.99) {
          Mineral.create(mineral, empty, mineralColor)
        } else {
          Rock.create(rock, empty)

        }
      } else if (d < 0.0) {
        Dirt.create(dirt, empty)
      } else {
        Empty(empty)
      }
      WorldTile(state)
    }
    Terrain(y, tiles)
  }

}
/* DONT FORGET TO ADD y TO SPAWN VALUES! */
case class Terrain private (y:Int, tiles:Array2d[WorldTile]) {
  def isSolid(s:(Int,Int)):Boolean = {
    val t = (tiles.getOption _).tupled(s)
    !t.exists{_.state.isWalkable}
  }

  def hit(ij:(Int,Int)):(Terrain, Seq[MineralDrop]) = {
    val t = tiles.get(ij.x, ij.y)
    val (newState, drops) = t.state match {
      case r@Rock(_,_,_) => r.hit
      case d@Dirt(_,_,_) => d.hit
      case e@Empty(_) => (e, Seq())
      case g@Mineral(_,_,_,_) => g.hit(ij.x, ij.y + y)
    }
    val newT = copy(tiles=tiles.update(ij.x, ij.y, _.copy(state=newState)))
    (newT, drops)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <++ tiles.flatten.map { case (i, j, w)  => (i, j+y, w.tile)}
  }
}
