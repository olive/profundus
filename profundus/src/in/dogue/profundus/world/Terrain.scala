package in.dogue.profundus.world

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.data.{Code, CP437, Array2d}
import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.MineralDrop
import in.dogue.antiqua.procgen.PerlinNoise
import in.dogue.antiqua.geometry.{Line, Ellipse}
import com.deweyvm.gleany.data.Point2d
import in.dogue.profundus.doodads.{Doodad, Moon}


case class Scheme(bgMod:Random => Color,
                  fgMod:Random => Color) {
  def getBg(r:Random) = bgMod(r)
  def getFg(r:Random) = fgMod(r)
  def mkTile(r:Random, c:Code) = c.mkTile(getBg(r), getFg(r))

}

case class TerrainScheme(sky:Scheme, grass:Scheme, dirt:Scheme, rock:Scheme, gem:Scheme) {

  private def makeEmpty(r:Random) = {
    val bgCode = Vector(CP437.`.`, CP437.`,`, CP437.`'`, CP437.`"`).randomR(r)
    rock.mkTile(r, bgCode)
  }

  def makeRock(r:Random) = {
    val rockTile = rock.mkTile(r, CP437.█)
    val empty = makeEmpty(r)
    Rock.create(rockTile, empty)
  }

  def makeDirt(r:Random) = {
    val dirtTile = dirt.mkTile(r, CP437.█)
    val empty = makeEmpty(r)
    Dirt.create(dirtTile, empty)

  }

  def makeMineral(r:Random) = {
    val mineralTile = gem.mkTile(r, CP437.◘)
    val empty = makeEmpty(r)
    Mineral.create(mineralTile, empty, mineralTile.bgColor)
  }

  def makeSky(r:Random) = {
    val skyCode = Vector((1, CP437.`.`), (1, CP437.`'`), (50, CP437.` `)).expand.randomR(r)
    val night = sky.mkTile(r, skyCode)
    Empty(night)
  }

  def makeGrass(yj:Int, rows:Int, r:Random) = {
    val grassTile = grass.mkTile(r, CP437.█)
    val empty = makeEmpty(r)
    Dirt.create(grassTile, empty)
  }
  type ChooseState = Int => Double => Random => TileState
  def generateRandom(cols:Int, rows:Int, y:Int, r:Random) = {

  }
}

object Terrain {
  val dirtScheme = Scheme(
    (r:Random) => Color.Brown.dim(3 + r.nextDouble),
    (r:Random) => Color.Tan.dim(3 + r.nextDouble)
  )
  val rockScheme = Scheme(
    (r:Random) => Color.Brown.dim(3 + r.nextDouble),
    (r:Random) => Color.Grey.dim(1 + r.nextDouble)
  )
  def grassScheme(j:Int, rows:Int) = Scheme(
    (r:Random) => Color.DarkGreen.mix(Color.Brown, j/rows.toDouble).dim(3 + r.nextDouble),
    (r:Random) => Color.DarkGreen.mix(Color.Brown, j/(rows*2).toDouble).dim(1 + r.nextDouble)
  )

  def skyScheme(dim:Double) = Scheme(
    (r:Random) => Color.DarkBlue.dim(1/dim),
    (r:Random) => Color.White
  )

  val gemScheme = Scheme(
    (r:Random) => Vector(Color.Purple, Color.Red, Color.Green, Color.Blue).randomR(r),
    (r:Random) => Color.Grey.dim(1 + r.nextDouble)
  )

  def createCave(y:Int, cols:Int, rows:Int, r:Random) = {
    val noise = new PerlinNoise().generate(cols, rows, 0, y, r.nextInt())
    val tiles = noise.map { case (i, j, d) =>
      val bgCode = Vector(CP437.`.`, CP437.`,`, CP437.`'`, CP437.`"`).randomR(r)
      val rock = rockScheme.mkTile(r, CP437.█)
      val dirt = dirtScheme.mkTile(r, CP437.█)
      val empty = rockScheme.mkTile(r, bgCode)
      val mineral = gemScheme.mkTile(r, CP437.◘)
      val state = if (d < -0.4) {
        if (r.nextDouble > 0.99) {
          Mineral.create(mineral, empty, mineral.bgColor)
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
    Terrain(y, tiles, Seq(), (0,0))
  }

  def createSky(y:Int, cols:Int, rows:Int, r:Random):Terrain = {
    val noise = new PerlinNoise().generate(cols, rows, 0, y, r.nextInt())
    val ellipse = Ellipse(cols/4, rows/2, cols/2, 3*rows/4)
    val sx = rows / 2 - 3
    val (cxx, cyy) = ellipse.center
    val (cx, cy) = (cxx.toInt, cyy.toInt - 5)
    val sy = ellipse.getY(sx).toVector.randomR(r).toInt
    val (sx2, sy2) = ((ellipse.cx - sx).toInt, (ellipse.cy + sy).toInt)
    val l1 = Line.bresenham(sx,  sy,      cx, cy)
    val l2 = Line.bresenham(sx,  sy + 1,  cx, cy + 1)
    val l3 = Line.bresenham(sx,  sy + 2,  cx, cy + 2)
    val l4 = Line.bresenham(sx2, sy2,     cx, cy)
    val l5 = Line.bresenham(sx2, sy2 + 1, cx, cy + 1)
    val l6 = Line.bresenham(sx2, sy2 + 2, cx, cy + 2)
    val lines = Vector(l1, l2, l3, l4, l5, l6)
    val tiles = noise.map { case (i, j, d) =>
      val bgCode = Vector(CP437.`.`, CP437.`,`, CP437.`'`, CP437.`"`).randomR(r)
      val skyCode = Vector((1, CP437.`.`), (1, CP437.`'`), (50, CP437.` `)).expand.randomR(r)
      val dim = (j + y) / (cols*2).toDouble
      val night = skyScheme(dim).mkTile(r, skyCode)
      val grass = grassScheme(j, rows).mkTile(r, CP437.█)
      val empty = rockScheme.mkTile(r, bgCode)
      val rock = rockScheme.mkTile(r, CP437.█)
      val dirt = dirtScheme.mkTile(r, CP437.█)
      val pt = (i, j+y)
      val state =
        if (lines.exists{_.contains(pt)} && y == 0) {
          Empty(empty)
        } else if (ellipse.contains((i, j)) && y == 0) {
          Dirt.create(dirt, empty)
        } else if (j + y > 16) {
          if (d > 0 || j + y < 20) {
            Dirt.create(grass, empty)
          } else if (d < -0.4) {
            Rock.create(rock, empty)
          } else {
            Dirt.create(dirt, empty)
          }
        } else {
          Empty(night)
        }
      WorldTile(state)
    }
    val moon = Moon.create(22, 0, 4)
    Terrain(y, tiles, Seq(moon.toDoodad), (sx, sy+2))
  }


}
/* DONT FORGET TO ADD y TO SPAWN VALUES! */
case class Terrain private (y:Int, tiles:Array2d[WorldTile], doodads:Seq[Doodad[_]], spawn:(Int,Int)) {
  def update = copy(doodads=doodads.map{_.update})
  def isSolid(s:(Int,Int)):Boolean = {
    val t = (tiles.getOption _).tupled(s)
    !t.exists{_.state.isWalkable}
  }

  def hit(ij:(Int,Int)):(Terrain, Seq[MineralDrop], Int) = {
    val t = tiles.get(ij.x, ij.y)
    val (newState, drops, damage) = t.state.hit(ij +| y)
    val newT = copy(tiles=tiles.update(ij.x, ij.y, _.copy(state=newState)))
    (newT, drops, damage)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <++ tiles.flatten.map {
      case (i, j, w) => (i, j + y, w.tile)
    }
  }

}
