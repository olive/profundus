package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Implicits
import Implicits._
import in.dogue.antiqua.data.Direction

object World {
  def create(cols:Int, rows:Int, r:Random) = {
    val copy = new Random(r.nextInt())
    def gen(i:Int, r:Random, prev:Terrain) = {
      Terrain.create(cols, rows, copy)
    }
    World(cols, rows, Map(0->Terrain.create(cols, rows, r)), 0, 0, gen, r)
  }
}

case class World(cols:Int, rows:Int, tMap:Map[Int, Terrain], max:Int, min:Int, mkNext:(Int, Random, Terrain) => Terrain, r:Random) {
  def isSolid(ij:(Int,Int)):Boolean = {
    get(ij).isSolid(convert(ij))
  }

  def isGrounded(ij:(Int,Int)):Boolean = {
    val down = ij --> Direction.Down
    get(down).isSolid(convert(down))
  }

  private def convert(ij:(Int,Int)):(Int,Int) = {
    (ij.x, ij.y %% rows)
  }


  def break(ij:(Int,Int)):World = {
    val index = getIndex(ij)
    val broke = tMap(index).break(convert(ij))
    val updated = tMap.updated(index, broke)
    copy(tMap=updated)
  }

  private def getIndex(ij:(Int,Int)) = {
    val y = ij.y
    val yy = (y < 0).select(y, y-(rows-1))
    yy/rows
  }

  def checkPositions(ij:(Int,Int)):World = {
    val index = getIndex(ij)
    check(index+1).check(index-1)
  }

  private def check(i:Int):World = {
    val (newMap, newMin, newMax) = if (i > max) {
      val mm = ((max+1) to i).foldLeft(tMap) { case (map, k) =>
        val prev = map(k-1)
        val next = mkNext(k, r, prev)
        map.updated(k, next)
      }
      (mm, min, i)
    } else if (i < min) {
      val mm = ((min - 1) to (i, -1)).foldLeft(tMap) { case (map, k) =>
        val prev = map(k+1)
        val next = mkNext(k, r, prev)
        map.updated(k, next)
      }
      (mm, i, max)
    } else {
      (tMap, min, max)
    }
    copy(tMap=newMap, min=newMin, max=newMax)
  }

  private def get(ij:(Int,Int)):Terrain = {
    tMap(getIndex(ij))

  }

  def draw(ij:(Int,Int))(t:TileRenderer):TileRenderer = {
    val y1 = ij.y %% rows
    val plOffset = rows/2 + 5
    val m = -y1-plOffset
    //optimization: only draw the two on screen
    val things = Vector(
      (tMap(getIndex(ij)-1), m - rows),
      (tMap(getIndex(ij)), m),
      (tMap(getIndex(ij)+1), m + rows)
    )

    things.foldLeft(t) { case (tr, (ter,mv)) =>
      tr.withMove(0, mv) { _ <+< ter.draw }
    }
  }
}
