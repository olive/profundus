package in.dogue.profundus.world

import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.data._
import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.Game
import in.dogue.antiqua.data.FutureFinished
import in.dogue.antiqua.data.FutureError
import in.dogue.antiqua.geometry.Line
import in.dogue.profundus.entities.{ToolType, Damage}

object NewTerrainCache {
  def foldFutures(tc:NewTerrainCache) = {
    tc.fs.foldLeft(tc) { case (ntc, f) =>
      f.update match {
        case FutureComputing => ntc
        case FutureError(msg) => throw new RuntimeException("Failed to load chunk.\n" + msg)
        case FutureFinished((i, s, t, ws)) => ntc.insert(i, s, t, ws)
      }
    }
  }

  def create(cols:Int, rows:Int, r:Random):(NewTerrainCache, Cell, Direction, Seq[WorldSpawn]) = {
    val copy = new Random(r.nextInt())
    val biome = Stratum.createSurface(r)
    val (biomep, first, gs) = biome.generate(cols, rows, 0, copy)
    val cache = TerrainCache(cols, rows, Map(0->(biomep, first)), Seq(), Seq(), r)
    (cache, first.spawn, first.spawnFace, gs)
  }
}

case class NewTerrainCache(cols:Int, rows:Int, tMap:Map[Int,(Stratum, Terrain)], fs:Seq[Future[(Int, Stratum,Terrain, Seq[WorldSpawn])]], dummy:Terrain, queuedSpawns:Seq[WorldSpawn], r:Random) {


  def isSolid(ij:Cell):Boolean = {
    get(ij).isSolid(toTerrainCoords(ij))
  }

  def isBackgroundSolid(ij:Cell):Boolean = {
    get(ij).isBackgroundSolid(toTerrainCoords(ij))
  }

  def isGrounded(ij:Cell):Boolean = {
    val down = ij --> Direction.Down
    get(down).isSolid(toTerrainCoords(down))
  }

  def isLoaded(ij:Cell):Boolean = {
    tMap.contains(getIndex(ij --> Direction.Down))
  }

  def hasLineOfSight(src:Cell, dst:Cell) = {
    val points = Line.bresenham(src.x, src.y, dst.x, dst.y)
    !points.exists { p => isSolid(p)}
  }

  def isRock(ij:Cell):Boolean ={
    get(ij).isRock(toTerrainCoords(ij))
  }

  def mineralize(ij:Cell):NewTerrainCache = {
    val index = getIndex(ij)
    val converted = toTerrainCoords(ij)
    val mineraled = get(ij).mineralize(converted)
    updateTerrain(index, mineraled)
  }

  private def updateTerrain(tIndex:Int, t:Terrain) = {
    val (s, _) = tMap(tIndex)
    val ntMap = tMap.updated(tIndex, (s, t))
    copy(tMap = ntMap)
  }

  def getTouching(ij:Cell):Direction => Option[WorldTile] = {
    def g(p:Cell) = {
      val terrain = get(p)
      val converted = toTerrainCoords(p)
      val opt = terrain.tiles.getOption(converted)
      opt.filter{ !_.isWalkable }.onlyIf(isLoaded(p)).flatten
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


  def hit(ij:Cell, dmg:Damage, ttype:ToolType):(NewTerrainCache, Seq[WorldSpawn], HitResult) = {
    val (broke, dropped, result) = get(ij).hit(toTerrainCoords(ij), dmg, ttype)
    val index = getIndex(ij)
    (updateTerrain(index, broke), dropped, result)
  }

  private def insert(i:Int, s:Stratum, t:Terrain, ws:Seq[WorldSpawn]) = {
    copy(tMap=tMap.updated(i, (s, t)), queuedSpawns=queuedSpawns++ws)
  }

  private def addFuture(f:Future[(Int, Stratum, Terrain, Seq[WorldSpawn])]) = {
    copy(fs=f+:fs)
  }

  def update(ppos:Cell):(NewTerrainCache, Seq[WorldSpawn]) = {
    val newI = getIndex(ppos)
    def gen(k:Int) = {
      val (prevS, _) = getAdjacent(k)
      val (stratum, terrain, spawns) = prevS.generate(cols, rows, k, r)
      (k, stratum, terrain, spawns)
    }

    val uCache = NewTerrainCache.foldFutures(this)

    val nCache = Seq(-1, 0, 1).map {_ + newI}.foldLeft(uCache) { case (ntc, index) =>
      val (_, stratum, terrain, spawns) = gen(index)
      insert(index, stratum, terrain, spawns)
    }

    val fCache = Seq(-2, 2).map {_ + newI}.foldLeft(nCache) { case (ntc, index) =>
      val f = new Future(() => gen(index))
      ntc.addFuture(f)
    }
    fCache.copy(queuedSpawns=Seq()) @@ fCache.queuedSpawns
  }



  def getAdjacent(i:Int):(Stratum, Terrain) = {
    (1 until 999) map { k =>
      if (tMap.contains(i+k)) {
        return tMap(i+k)
      } else if (tMap.contains(i-k)) {
        return tMap(i-k)
      }
    }
    throw new RuntimeException("Too far from known coordinates!")
  }

  private def getRaw(i:Int) = {
    tMap.get(i).map {_._2}.getOrElse(dummy)
  }

  private def get(ij:Cell) = {
    getRaw(getIndex(ij))
  }

  private def getIndex(ij:Cell) = {
    val y = ij.y
    val yy = (y < 0).select(y, y-(rows-1))
    yy/rows
  }


  private def toTerrainCoords(ij:Cell):Cell = {
    (ij.x, ij.y %% rows)
  }

  def draw(ij:Cell)(t:TileRenderer):TileRenderer = {
    val ts = Vector(-1, 0, 1).map { k => getRaw(getIndex(ij) + k) }

    val onScreen = ts.filter { ter => t.project(ter.getRect).intersects(Recti(0,0,32, 48))}
    Game.drawPerf.track("terrain") {
      onScreen.foldLeft(t) { _ <+< _.draw }
    }
  }
}
