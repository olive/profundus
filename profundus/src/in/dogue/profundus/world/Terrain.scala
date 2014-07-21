package in.dogue.profundus.world

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.data.{Direction,CP437, Array2d}
import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.procgen.PerlinNoise
import in.dogue.antiqua.geometry.{Circle, Line}
import in.dogue.profundus.doodads.{Campfire, Doodad, Moon}
import in.dogue.profundus.entities.pickups.Pickup
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.world.features.SpikePit
import in.dogue.profundus.entities.ToolType


object Terrain {

  def skyFeature(cols:Int, rows:Int) = Feature(Recti(0,0,cols, rows), createSky)

  def createSky(cols:Int, rows:Int, y:Int, ts:TerrainScheme, tiles:Array2d[WorldTile], r:Random) = {
    val noise = new PerlinNoise().generate(cols, rows, 0, y, r.nextInt())

    def skyScheme(dim:Double) = Scheme(
      (r:Random) => Color.DarkBlue.dim(1/dim),
      (r:Random) => Color.White
    )
    def makeSky(dim:Double)(r:Random) = {
      val skyCode = Vector((1, CP437.`.`), (1, CP437.`'`), (50, CP437.` `)).expand.randomR(r)
      val night = skyScheme(dim).mkTile(r, skyCode)
      Empty(night, false)
    }

    val tiles = noise.map { case (i, j, d) =>
      val dim = (j + y*rows) / (cols*2).toDouble
      val night = makeSky(dim) _

      WorldTile(night(r))
    }

    (tiles, Seq())
  }


  def makeLines(cols:Int, rows:Int, r:Random) = {
    val circle = Circle((cols/2, rows/2), rows/4)
    val pi = Math.PI
    val base = pi/8
    val (face, angle) = Vector((Direction.Right, r.nextDouble * base + pi),
                               (Direction.Left, 2*pi - r.nextDouble * base)).randomR(r)
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
    val lines: Vector[Seq[(Int, Int)]] = Vector(l1, l2, l3, l4, l5, l6)
    ((l2(0).x, l2(0).y), face, lines, circle)
  }

  def createMouth(face:Direction, lines:Vector[Seq[Cell]], circle:Circle)(cols:Int, rows:Int, y:Int, ts:TerrainScheme, tiles:Array2d[WorldTile], r:Random)
      : (Array2d[WorldTile], Seq[Doodad[_]]) = {
    val noise = new PerlinNoise().generate(cols, rows, 0, y, r.nextInt())


    val scheme = TerrainScheme.dummy
    val grassScheme = Scheme(
      (r:Random) => Color.DarkGreen.mix(Color.Brown, r.nextDouble/6).dim(3 + r.nextDouble),
      (r:Random) => Color.DarkGreen.mix(Color.Brown, r.nextDouble/6).dim(1 + r.nextDouble)
    )

    def skyScheme(dim:Double) = Scheme(
      (r:Random) => Color.DarkBlue.dim(1/dim),
      (r:Random) => Color.White
    )
    def makeSky(dim:Double)(r:Random) = {
      val skyCode = Vector((1, CP437.`.`), (1, CP437.`'`), (50, CP437.` `)).expand.randomR(r)
      val night = skyScheme(dim).mkTile(r, skyCode)
      Empty(night, false)
    }

    def makeGrass(yj:Int, rows:Int)(r:Random) = {
      val grassTile = grassScheme.mkTile(r, CP437.█)
      val empty = scheme.emptyTile(r)
      Dirt.create(grassTile, empty)
    }
    val tiles = noise.map { case (i, j, d) =>
      val dim = (j + y) / (cols*2).toDouble
      val night = makeSky(dim) _
      val grass = makeGrass(j + y, rows) _
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
    val moon = Moon.create(cols, rows, 3*cols/4-5, -5, 4)
    val campX = if (face == Direction.Right) 2*cols/6 else 4*cols/6
    val campfire = Campfire.create((campX, rows/2))
    (tiles, Seq(moon.toDoodad, campfire.toDoodad))
  }


}
/* DONT FORGET TO ADD y TO SPAWN VALUES! */
case class Terrain(y:Int, tiles:Array2d[WorldTile], doodads:Seq[Doodad[T] forSome {type T}], spawn:Cell, spawnFace:Direction) {
  def update = {
    val ds = doodads.map{_.update}
    val lights = doodads.map{_.getLight}
    (copy(doodads=ds), lights)
  }
  def isSolid(s:Cell):Boolean = {
    val t = (tiles.getOption _).tupled(s)
    !t.exists{_.state.isWalkable}
  }

  def isBackgroundSolid(s:Cell):Boolean = {
    val t = (tiles.getOption _).tupled(s)
    t.exists{_.state.bgSolid}
  }


  def hit(ij:Cell, dmg:Int, ttype:ToolType):(Terrain, Seq[WorldSpawn], Int, Boolean) = {
    val t = tiles.get(ij.x, ij.y)
    if (!t.canBreakBy(ttype.breakable)) {
      (this, Seq(), 0, false)
    } else {
      val (newState, drops, damage, broken) = t.state.hit(ij +| y, dmg)
      val newT = copy(tiles=tiles.update(ij.x, ij.y, _.copy(state=newState)))
      (newT, drops, damage, broken)
    }

  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <++ tiles.flatten.map {
      case (i, j, w) => (i, j + y, w.tile)
    }
  }

}
