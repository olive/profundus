package in.dogue.profundus.world

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.data.{Direction, Code, CP437, Array2d}
import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.MineralDrop
import in.dogue.antiqua.procgen.PerlinNoise
import in.dogue.antiqua.geometry.{Circle, Line, Ellipse}
import com.deweyvm.gleany.data.Point2d
import in.dogue.profundus.doodads.{Doodad, Moon}


case class Scheme(bgMod:Random => Color,
                  fgMod:Random => Color) {
  def getBg(r:Random) = bgMod(r)
  def getFg(r:Random) = fgMod(r)
  def mkTile(r:Random, c:Code) = c.mkTile(getBg(r), getFg(r))

}

case class TerrainScheme(sky:Double => Scheme, grass:Scheme, dirt:Scheme, rock:Scheme, rock2:Scheme, rock3:Scheme, gem:Scheme, clay:Scheme, empty:Scheme) {

  private def emptyTile(r:Random) = {
    val bgCode = Vector(CP437.`.`, CP437.`,`, CP437.`'`, CP437.`"`).randomR(r)
    empty.mkTile(r, bgCode)
  }

  def makeEmpty(r:Random) = {
    val empty = emptyTile(r)
    Empty(empty)
  }

  def makeClay(r:Random) = {
    val clayTile = clay.mkTile(r, CP437.-)
    val empty = emptyTile(r)
    Clay.create(clayTile, empty)
  }

  def `make???`(r:Random) = {
    val tile = ??? //.mkTile(r, CP437.⌂)
    ???
  }

  def makeRock3(r:Random) = {
    val rockTile = rock3.mkTile(r, CP437.`#`)
    val empty = emptyTile(r)
    Rock3.create(rockTile, empty)
  }

  def makeRock2(r:Random) = {
    val rockTile = rock2.mkTile(r, CP437.≡)
    val empty = emptyTile(r)
    Rock2.create(rockTile, empty)
  }

  def makeRock(r:Random) = {
    val rockTile = rock.mkTile(r, CP437.`=`)
    val empty = emptyTile(r)
    Rock.create(rockTile, empty)
  }

  def makeDirt(r:Random) = {
    val dirtTile = dirt.mkTile(r, CP437.█)
    val empty = emptyTile(r)
    Dirt.create(dirtTile, empty)

  }

  def makeMineral(r:Random) = {
    val mineralTile = gem.mkTile(r, CP437.◘)
    val empty = emptyTile(r)
    Mineral.create(mineralTile, empty, mineralTile.bgColor)
  }

  def makeSky(dim:Double)(r:Random) = {
    val skyCode = Vector((1, CP437.`.`), (1, CP437.`'`), (50, CP437.` `)).expand.randomR(r)
    val night = sky(dim).mkTile(r, skyCode)
    Empty(night)
  }

  def makeGrass(yj:Int, rows:Int)(r:Random) = {
    val grassTile = grass.mkTile(r, CP437.█)
    val empty = emptyTile(r)
    Dirt.create(grassTile, empty)
  }

}

object Terrain {
  val dirtScheme = Scheme(
    (r:Random) => Color.Brown.dim(6 + r.nextDouble),
    (r:Random) => Color.Brown.dim(3 + r.nextDouble)
  )

  val emptyScheme = Scheme(
    (r:Random) => Color.Tan.dim(2 + r.nextDouble),
    (r:Random) => Color.Tan.dim(1 + r.nextDouble)
  )
  val rockScheme = Scheme(
    (r:Random) => Color.Grey.dim(6 + r.nextDouble),
    (r:Random) => Color.Grey.dim(3 + r.nextDouble)
  )


  val rock2Scheme = Scheme(
    (r:Random) => Color.Grey.dim(10 + r.nextDouble),
    (r:Random) => Color.Grey.dim(6 + r.nextDouble)
  )

  val rock3Scheme = Scheme(
    (r:Random) => Color.Green.dim(6 + r.nextDouble),
    (r:Random) => Color.Green.dim(3 + r.nextDouble)
  )

  val grassScheme = Scheme(
    (r:Random) => Color.DarkGreen.mix(Color.Brown, r.nextDouble/6).dim(3 + r.nextDouble),
    (r:Random) => Color.DarkGreen.mix(Color.Brown, r.nextDouble/6).dim(1 + r.nextDouble)
  )

  def skyScheme(dim:Double) = Scheme(
    (r:Random) => Color.DarkBlue.dim(1/dim),
    (r:Random) => Color.White
  )

  val gemScheme = Scheme(
    (r:Random) => Vector(Color.Purple, Color.Red, Color.Green, Color.Blue).randomR(r),
    (r:Random) => Color.Grey.dim(3 + r.nextDouble)
  )

  def r2(r:Random) = {
    val x = r.nextDouble
    (1 - x)*(1 - x)
  }
  val clayScheme = Scheme(
    (r:Random) => Color.Red.mix(Color.Brown, r2(r)).dim(6 + r.nextDouble),
    (r:Random) => Color.Red.mix(Color.Brown, r2(r)).dim(3 + r.nextDouble)
  )

  val scheme = TerrainScheme(skyScheme, grassScheme, dirtScheme, rockScheme, rock2Scheme, rock3Scheme, gemScheme, clayScheme, emptyScheme)

  def createCave(y:Int, cols:Int, rows:Int, r:Random) = {
    val noise = new PerlinNoise().generate(cols, rows, 0, y, r.nextInt())
    val tiles = noise.map { case (i, j, d) =>
      val rock = scheme.makeRock _
      val rock2 = scheme.makeRock2 _
      val rock3 = scheme.makeRock3 _
      val dirt = scheme.makeDirt _
      val clay = scheme.makeClay _
      val mineral = scheme.makeMineral _
      val empty = scheme.makeEmpty _
      val state = if (d < -0.2) {
        if (r.nextDouble > 0.99) {
          if (r.nextBoolean) {
            rock3
          } else {
            mineral
          }

        } else if (d < -0.6) {
          rock2
        } else if (d < -0.4){
          rock
        } else {
          clay
        }
      } else if (d < 0.0) {
        dirt
      } else {
        empty
      }
      WorldTile(state(r))
    }
    placeSpikes(Terrain(y, tiles, Seq(), (0,0)), r)
  }

  def createSky(y:Int, cols:Int, rows:Int, r:Random):Terrain = {
    val noise = new PerlinNoise().generate(cols, rows, 0, y, r.nextInt())
    val circle = Circle((cols/2, rows/2), rows/4)
    val pi = Math.PI
    val base = pi/8
    val angle = Vector(r.nextDouble * base + pi, 2*pi - r.nextDouble * base).randomR(r)
    val upper = circle.angleToEdge(angle)
    val l1 = Line.bresenham(upper.x, upper.y - 1, circle.x, circle.y - 1)
    val l2 = Line.bresenham(upper.x, upper.y    , circle.x, circle.y)
    val angle2 = r.nextDouble()*pi/4 + pi/2
    val lower = circle.angleToEdge(angle2)
    val l3 = Line.bresenham(circle.x,   circle.y, lower.x, lower.y)
    val l4 = Line.bresenham(circle.x+1, circle.y, lower.x+1, lower.y)
    val finalX = 4 + r.nextInt(cols-8)
    val l5 = Line.bresenham(lower.x, lower.y, finalX, rows)
    val l6 = Line.bresenham(lower.x+1, lower.y, finalX+1, rows)
    val lines = Vector(l1, l2, l3, l4, l5, l6)
    val tiles = noise.map { case (i, j, d) =>
      val dim = (j + y) / (cols*2).toDouble
      val night = scheme.makeSky(dim) _
      val grass = scheme.makeGrass(j + y, rows) _
      val empty = scheme.makeEmpty _
      val rock = scheme.makeRock _
      val dirt = scheme.makeDirt _
      val pt = (i, j+y)
      val state =
        if (lines.exists{_.contains(pt)} && y == 0) {
          empty
        } else if (circle.contains((i, j)) && y == 0) {
          dirt
        } else if (j + y > rows/2) {
          if (d > 0 || j + y < rows/2 + 4) {
            grass
          } else if (d < -0.4) {
            rock
          } else {
            dirt
          }
        } else {
          night
        }
      WorldTile(state(r))
    }
    val moon = Moon.create(3*cols/4-5, -5, 4)
    Terrain(y, tiles, Seq(moon.toDoodad), (l1(0).x, l1(0).y))
  }

  def placeSpikes(terrain:Terrain, r:Random):Terrain = {
    def get(ij:(Int,Int)):Boolean = terrain.tiles.getOption(ij.x, ij.y).exists{_.isWalkable}
    def mkSpike(d:Direction, r:Random):WorldTile = {
      import Direction._
      val code = d match {
        case Left => CP437.◄
        case Right => CP437.►
        case Down => CP437.▼
        case Up => CP437.▲
      }
      val empty = emptyScheme.getBg(r)
      val tile: Tile = code.mkTile(empty, Color.White)
      val emptyTile = CP437.` `.mkTile(empty, empty)
      WorldTile(Spike.create(tile, emptyTile, d))
    }
    val ts = terrain.tiles.map { case (i, j, t) =>
      val p = (i, j)
      val left = get(p |- 1)
      val right = get(p |+ 1)
      val down = get(p +| 1)
      val up = get(p -| 1)
      if (r.nextDouble > 0.9) {
        if (down && !up && t.isWalkable) {
          mkSpike(Direction.Down, r)
        } else if (up && !down && t.isWalkable) {
          mkSpike(Direction.Up, r)
        } else {
          t
        }

      } else {
        t
      }
    }

    terrain.copy(tiles=ts)
  }


}
/* DONT FORGET TO ADD y TO SPAWN VALUES! */
case class Terrain private (y:Int, tiles:Array2d[WorldTile], doodads:Seq[Doodad[_]], spawn:(Int,Int)) {
  def update = copy(doodads=doodads.map{_.update})
  def isSolid(s:(Int,Int)):Boolean = {
    val t = (tiles.getOption _).tupled(s)
    !t.exists{_.state.isWalkable}
  }


  def hit(ij:(Int,Int), dmg:Int):(Terrain, Seq[MineralDrop], Int, Boolean) = {
    val t = tiles.get(ij.x, ij.y)
    val (newState, drops, damage, broken) = t.state.hit(ij +| y, dmg)
    val newT = copy(tiles=tiles.update(ij.x, ij.y, _.copy(state=newState)))
    (newT, drops, damage, broken)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <++ tiles.flatten.map {
      case (i, j, w) => (i, j + y, w.tile)
    }
  }

}
