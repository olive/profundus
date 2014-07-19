package in.dogue.profundus.world

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.data.{Direction,CP437, Array2d}
import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.procgen.PerlinNoise
import in.dogue.antiqua.geometry.{Circle, Line}
import in.dogue.profundus.doodads.{Doodad, Moon}
import in.dogue.profundus.entities.pickups.Pickup


object Terrain {

  def upper(scheme:TerrainScheme)(i:Int, j:Int, y:Int, cols:Int, rows:Int, d:Double, r:Random) = {
    val func = if (i <= 0 || (i <= 1 && r.nextDouble < 0.6) || i >= cols - 1 || (i >= cols - 2 && r.nextDouble < 0.6)) {
      scheme.makeShaft _
    } else if (d < -0.2) {
      if (r.nextDouble > 0.99) {
        if (r.nextBoolean) {
          scheme.makeRock3 _
        } else {
          scheme.makeMineral _
        }
      } else if (d < -0.6) {
        scheme.makeRock2 _
      } else if (d < -0.4){
        scheme.makeRock _
      } else {
        scheme.makeClay _
      }
    } else if (d < 0.0) {
      scheme.makeDirt _
    } else {
      scheme.makeEmpty _
    }
    func
  }


  val shaftScheme = Scheme(
    (r:Random) => Color.Grey.dim(12 + r.nextDouble),
    (r:Random) => Color.Grey.dim(9 + r.nextDouble)
  )

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

  val scheme = TerrainScheme(dirtScheme, rockScheme, rock2Scheme, rock3Scheme, gemScheme, clayScheme, shaftScheme, emptyScheme)

  def createCave(y:Int, cols:Int, rows:Int, r:Random) = {
    val mix = ((y/rows.toDouble - 1)/50).clamp(0, 0.5)
    val sche = scheme.map(c => c.mix(Color.Blue, mix))
    val gen = {
      TerrainGenerator(sche, upper(sche))
    }
    val noise = new PerlinNoise().generate(cols, rows, 0, y, r.nextInt())
    val tiles = noise.map { case (i, j, d) =>
      val state = gen.mkTile(i, j, y, cols, rows, d, r)
      WorldTile(state(r))
    }
    val spike = placeSpikes(sche, Terrain(y, tiles, Seq(), (0,0), Direction.Down), r)
    val width = 5
    val height = 10
    val xx = r.nextInt(cols - width)
    val yy = r.nextInt(rows - height)
    val shaft = SpikePit(xx, yy, width, height).toFeature(cols, rows, sche)
    shaft.transform(spike, r)
  }

  def createSky(y:Int, cols:Int, rows:Int, r:Random):Terrain = {
    val noise = new PerlinNoise().generate(cols, rows, 0, y, r.nextInt())
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
    val lines = Vector(l1, l2, l3, l4, l5, l6)
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
    val moon = Moon.create(3*cols/4-5, -5, 4)
    Terrain(y, tiles, Seq(moon.toDoodad), (l2(0).x, l2(0).y), face)
  }

  def placeSpikes(scheme:TerrainScheme, terrain:Terrain, r:Random):Terrain = {
    def get(ij:Cell):Boolean = terrain.tiles.getOption(ij.x, ij.y).exists{_.isWalkable}

    val ts = terrain.tiles.map { case (i, j, t) =>
      val p = (i, j)
      val left = get(p |- 1)
      val right = get(p |+ 1)
      val down = get(p +| 1)
      val up = get(p -| 1)
      if (r.nextDouble > 0.9) {
        if (down && !up && t.isWalkable) {
          WorldTile(scheme.makeSpike(Direction.Down)(r))
        } else if (up && !down && t.isWalkable) {
          WorldTile(scheme.makeSpike(Direction.Up)(r))
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
case class Terrain private (y:Int, tiles:Array2d[WorldTile], doodads:Seq[Doodad[_]], spawn:Cell, spawnFace:Direction) {
  def update = copy(doodads=doodads.map{_.update})
  def isSolid(s:Cell):Boolean = {
    val t = (tiles.getOption _).tupled(s)
    !t.exists{_.state.isWalkable}
  }

  def isBackgroundSolid(s:Cell):Boolean = {
    val t = (tiles.getOption _).tupled(s)
    t.exists{_.state.bgSolid}
  }


  def hit(ij:Cell, dmg:Int):(Terrain, Seq[Pickup[_]], Int, Boolean) = {
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
