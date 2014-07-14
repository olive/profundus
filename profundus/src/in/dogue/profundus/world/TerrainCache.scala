package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.entities.MineralDrop
import in.dogue.antiqua.geometry.Line


object TerrainCache {
  def create(cols:Int, rows:Int, r:Random):(TerrainCache,(Int,Int)) = {
    val copy = new Random(r.nextInt())
    def gen(i:Int, r:Random, prev:Option[Terrain]) = {
      val gen = if (i <= 0) {
        Terrain.createSky _
      } else {
        Terrain.createCave _
      }
      gen(i*rows, cols, rows, copy)
    }
    val first = gen(0, r, None)
    val cache = TerrainCache(cols, rows, Map(0->first), 0, 0, gen, r)
    (cache, first.spawn)
  }
}
case class TerrainCache private (cols:Int, rows:Int,
                                 tMap:Map[Int, Terrain], max:Int, min:Int,
                                 mkNext:(Int, Random, Option[Terrain]) => Terrain,
                                 r:Random) {
  def isSolid(ij:(Int,Int)):Boolean = {
    get(ij).isSolid(convert(ij))
  }

  def isGrounded(ij:(Int,Int)):Boolean = {
    val down = ij --> Direction.Down
    get(down).isSolid(convert(down))
  }

  def isLoaded(ij:(Int,Int)):Boolean = {
    tMap.contains(getIndex(ij))
  }

  def hasLineOfSight(src:(Int,Int), dst:(Int,Int)) = {
    val points = Line.bresenham(src.x, src.y, dst.x, dst.y)
    !points.exists { p => isSolid(p)}
  }

  private def convert(ij:(Int,Int)):(Int,Int) = {
    (ij.x, ij.y %% rows)
  }


  def hit(ij:(Int,Int)):(TerrainCache, Seq[MineralDrop], Int) = {
    val index = getIndex(ij)
    val (broke, dropped, damage) = tMap(index).hit(convert(ij))
    val updated = tMap.updated(index, broke)
    (copy(tMap=updated), dropped, damage)
  }

  private def getIndex(ij:(Int,Int)) = {
    val y = ij.y
    val yy = (y < 0).select(y, y-(rows-1))
    yy/rows
  }

  def checkPositions(ij:(Int,Int)):TerrainCache = {
    val index = getIndex(ij)
    check(index+2).check(index+1).check(index-1)
  }

  private def check(i:Int):TerrainCache = {
    val (newMap, newMin, newMax) = if (i > max) {
      val mm = ((max+1) to i).foldLeft(tMap) { case (map, k) =>
        val prev = map(k-1)
        val next = mkNext(k, r, prev.some)
        map.updated(k, next)
      }
      (mm, min, i)
    } else if (i < min) {
      val mm = ((min - 1) to (i, -1)).foldLeft(tMap) { case (map, k) =>
        val prev = map(k+1)
        val next = mkNext(k, r, prev.some)
        map.updated(k, next)
      }
      (mm, i, max)
    } else {
      (tMap, min, max)
    }
    copy(tMap=newMap, min=newMin, max=newMax)
  }

  def update(ij:(Int,Int)):TerrainCache = {
    val newTMap = Seq(getIndex(ij)-1, getIndex(ij), getIndex(ij)+1).foldLeft(tMap) { case (acc, i) =>
      acc.updated(i, acc(i).update)
    }
    copy(tMap=newTMap)
  }

  private def get(ij:(Int,Int)):Terrain = {
    tMap(getIndex(ij))

  }

  def draw(ij:(Int,Int))(t:TileRenderer):TileRenderer = {
    //optimization: only draw the two on screen
    val things = Vector(
      tMap(getIndex(ij)-1),
      tMap(getIndex(ij)),
      tMap(getIndex(ij)+1)
    )

    val doodads = things.map{_.doodads}

    things.foldLeft(t) {   _ <+< _.draw } <++< doodads.flatten.map{_.draw _}
  }
}
