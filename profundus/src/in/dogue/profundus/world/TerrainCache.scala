package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.{Future, Direction}
import in.dogue.profundus.entities.{ToolType, Obelisk, Lurker}
import in.dogue.antiqua.geometry.Line
import in.dogue.profundus.entities.pickups.{FoodType, Toadstool, FoodPickup, Pickup}
import in.dogue.profundus.lighting.LightSource
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.Game


object TerrainCache {
  def create(cols:Int, rows:Int, r:Random):(TerrainCache,Cell,Direction, Seq[GlobalSpawn]) = {
    val copy = new Random(r.nextInt())
    val biome = Stratum.createSurface(r)
    val (biomep, first, gs) = biome.generate(cols, rows, 0, copy)
    val cache = TerrainCache(cols, rows, Map(0->first), 0, 0, biomep, r)
    (cache, first.spawn, first.spawnFace, gs)
  }
}
case class TerrainCache private (cols:Int, rows:Int,
                                 tMap:Map[Int, Terrain], max:Int, min:Int,
                                 biome:Stratum,
                                 r:Random) {
  def isSolid(ij:Cell):Boolean = {
    get(ij).map{_.isSolid(convert(ij))}.getOrElse(true)
  }

  def isBackgroundSolid(ij:Cell):Boolean = {
    get(ij).map{_.isBackgroundSolid(convert(ij))}.getOrElse(true)
  }

  def isGrounded(ij:Cell):Boolean = {
    val down = ij --> Direction.Down
    get(down).map{_.isSolid(convert(down))}.getOrElse(true)
  }

  def isLoaded(ij:Cell):Boolean = {
    tMap.contains(getIndex(ij --> Direction.Down))
  }

  def hasLineOfSight(src:Cell, dst:Cell) = {
    val points = Line.bresenham(src.x, src.y, dst.x, dst.y)
    !points.exists { p => isSolid(p)}
  }

  def isRock(ij:Cell):Boolean ={
    get(ij).exists{_.isRock(convert(ij))}
  }

  def mineralize(ij:Cell, seed:Int):TerrainCache = {
    val index = getIndex(ij)
    val converted = convert(ij)
    val mineraled = tMap(index).mineralize(converted, seed)
    copy(tMap=tMap.updated(index, mineraled))
  }

  def getTouching(ij:Cell):Direction => Option[WorldTile] = {
    def g(p:Cell) = {
      val terrain = get(p)
      terrain.map { t =>
        val converted = convert(p)
        val opt = t.tiles.getOption(converted)
        opt.filter{ !_.isWalkable }.onlyIf(isLoaded(p)).flatten

      }.getOrElse(None)
    }
    import Direction._
    def touching(d:Direction) = d match {
      case Down => g(ij +| 1)
      case Up => g(ij -| 1)
      case Left => g(ij |- 1)
      case Right => g(ij |+ 1)
    }
    touching

  }

  private def convert(ij:Cell):Cell = {
    (ij.x, ij.y %% rows)
  }


  def hit(ij:Cell, dmg:Int, ttype:ToolType):(TerrainCache, Seq[GlobalSpawn], Int, Boolean) = {
    val index = getIndex(ij)
    val (broke, dropped, damage, broken) = tMap(index).hit(convert(ij), dmg, ttype)
    val updated = tMap.updated(index, broke)
    (copy(tMap=updated), dropped, damage, broken)
  }

  private def getIndex(ij:Cell) = {
    val y = ij.y
    val yy = (y < 0).select(y, y-(rows-1))
    yy/rows
  }

  def checkPositions(ij:Cell):(TerrainCache, Seq[GlobalSpawn]) = {
    val index = getIndex(ij)
    val seed = (this, Seq[GlobalSpawn]())
    Seq(index-1, index+1, index+2).foldLeft(seed) { case ((map, gs), i) =>
      val (next, newGs) = map.check(i)
      (next, gs ++ newGs)
    }
  }

  //fixme -- code clones
  private def check(i:Int):(TerrainCache, Seq[GlobalSpawn]) = {

    val range = {
      if (i > max) {
        (max+1) to i
      } else if (i < min){
        (min - 1) to (i, -1)
      } else {
        Seq()
      }
    }



    val (newBiome, newMap, newMin, newMax, gs) = {
      val seed = (biome, tMap, Seq[GlobalSpawn]())
      //fixme -- use fold3
      val (b, mm, gs) = range.foldLeft(seed) { case ((bm, map, gs), k) =>
        val (newBiome, next, moreGs) = bm.generate(cols, rows, k, r)
        (newBiome, map.updated(k, next), moreGs ++ gs)
      }
      (b, mm, math.min(min, i), math.max(max, i), gs)
    }

    val newTc = copy(biome = newBiome, tMap=newMap, min=newMin, max=newMax)
    (newTc, gs)

  }

  private def get(ij:Cell):Option[Terrain] = {
    tMap.get(getIndex(ij))


  }

  def draw(ij:Cell)(t:TileRenderer):TileRenderer = {
    val things = Vector(
      tMap(getIndex(ij)-1),
      tMap(getIndex(ij)),
      tMap(getIndex(ij)+1)
    )

    val onScreen = things.filter { ter => t.project(ter.getRect).intersects(Recti(0,0,32, 48))}
    Game.drawPerf.track("terrain") {
      onScreen.foldLeft(t) { _ <+< _.draw }
    }
  }
}
