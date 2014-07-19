package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.entities.{Casque, Creature}
import in.dogue.antiqua.geometry.Line
import in.dogue.profundus.entities.pickups.{FoodType, Toadstool, FoodPickup, Pickup}


object TerrainCache {
  def create(cols:Int, rows:Int, r:Random):(TerrainCache,Cell,Direction) = {
    val copy = new Random(r.nextInt())
    def gen(i:Int, r:Random, prev:Option[Terrain]) = {
      val gen = if (i <= 0) {
        Terrain.createSky _
      } else {
        Terrain.createCave _
      }
      val t = gen(i*rows, cols, rows, copy)

      val made = (0 until 10) map { case _ =>
        val (x, y) = (r.nextInt(cols), r.nextInt(rows))
        Creature.create(x, y + i*rows)
      }

      val food=(0 until 10) map { case _ =>
        val (x, y) = (r.nextInt(cols), r.nextInt(rows))
        FoodPickup.create((x, y + i*rows), FoodType.random(r)).toPickup

      }
      val casques = (0 until 10) map { case _ =>
        val (x, y) = (r.nextInt(cols), r.nextInt(rows))
        Casque.create((x, y + i*rows), r)

      }
      val cs = (i > 0).select(Seq(), Seq(CasqueSpawn(casques), CreatureSpawn(made), PickupSpawn(food)))
      (t, cs)
    }
    val (first,_) = gen(0, r, None)
    val cache = TerrainCache(cols, rows, Map(0->first), 0, 0, gen, r)
    (cache, first.spawn, first.spawnFace)
  }
}
case class TerrainCache private (cols:Int, rows:Int,
                                 tMap:Map[Int, Terrain], max:Int, min:Int,
                                 mkNext:(Int, Random, Option[Terrain]) => (Terrain, Seq[WorldSpawn]),
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

  def getTouching(ij:Cell):Direction => Option[WorldTile] = {
    def g(p:Cell) = {
      val terrain = get(p)
      terrain.map { t =>
        val converted = convert(p)
        val opt = t.tiles.getOption(converted.x, converted.y)
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


  def hit(ij:Cell, dmg:Int):(TerrainCache, Seq[Pickup[_]], Int, Boolean) = {
    val index = getIndex(ij)
    val (broke, dropped, damage, broken) = tMap(index).hit(convert(ij), dmg)
    val updated = tMap.updated(index, broke)
    (copy(tMap=updated), dropped, damage, broken)
  }

  private def getIndex(ij:Cell) = {
    val y = ij.y
    val yy = (y < 0).select(y, y-(rows-1))
    yy/rows
  }

  def checkPositions(ij:Cell):(TerrainCache, Seq[WorldSpawn]) = {
    val index = getIndex(ij)
    val seed = (this, Seq[WorldSpawn]())
    Seq(index+2, index+1, index-1).foldLeft(seed) { case ((map, cs), i) =>
      val (next, newCs) = map.check(i)
      (next, cs ++ newCs)
    }
  }

  //fixme -- code clones
  private def check(i:Int):(TerrainCache, Seq[WorldSpawn]) = {
    val (newMap, newMin, newMax, cs) = if (i > max) {
      val seed = (tMap, Seq[WorldSpawn]())
      val (mm, cs) = ((max+1) to i).foldLeft(seed) { case ((map, cs), k) =>
        val prev = map(k-1)
        val (next, moreCs) = mkNext(k, r, prev.some)
        (map.updated(k, next), moreCs ++ cs)
      }
      (mm, min, i, cs)
    } else if (i < min) {
      val seed = (tMap, Seq[WorldSpawn]())
      val (mm, cs) = ((min - 1) to (i, -1)).foldLeft(seed) { case ((map, cs), k) =>
        val prev = map(k+1)
        val (next, moreCs)  = mkNext(k, r, prev.some)
        (map.updated(k, next), moreCs ++ cs)
      }
      (mm, i, max, cs)
    } else {
      (tMap, min, max, Seq())
    }
    val newTc = copy(tMap=newMap, min=newMin, max=newMax)
    (newTc, cs)
  }

  def update(ij:Cell):TerrainCache = {
    val newTMap = Seq(getIndex(ij)-1, getIndex(ij), getIndex(ij)+1).foldLeft(tMap) { case (acc, i) =>
      acc.updated(i, acc(i).update)
    }
    copy(tMap=newTMap)
  }

  private def get(ij:Cell):Option[Terrain] = {
    tMap.get(getIndex(ij))


  }

  def draw(ij:Cell)(t:TileRenderer):TileRenderer = {
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
