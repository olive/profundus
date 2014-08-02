package in.dogue.profundus.world

import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.data._
import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.Game
import in.dogue.antiqua.data.FutureFinished
import in.dogue.antiqua.data.FutureError
import in.dogue.antiqua.geometry.{Circle, Line}
import in.dogue.profundus.entities.{ToolType, Damage}
import in.dogue.antiqua.procgen.PerlinNoise
import scala.collection.script.Index
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import com.deweyvm.gleany.graphics.Color

object TerrainCache {
  def foldFutures(tc:TerrainCache) = {
    val seed = (tc, Map[Int,Future[(Stratum,Terrain, Seq[GlobalMessage])]]())
    tc.fs.foldLeft(seed) { case ((ntc, fs), (i, f)) =>
      f.update match {
        case FutureComputing => (ntc, fs.updated(i, f))
        case FutureError(msg) => throw new RuntimeException("Failed to load chunk.\n" + msg)
        case FutureFinished((s, t, ws)) => (ntc.insert(i, s, t, ws), fs)
      }
    }
  }

  def getAdjacent(tc:TerrainCache, i:Int):(Stratum, Terrain) = {
    (1 until 999) map { k =>
      if (tc.tMap.contains(i+k)) {
        return tc.tMap(i+k)
      } else if (tc.tMap.contains(i-k)) {
        return tc.tMap(i-k)
      }
    }
    tc.tMap(i)
  }

  def gen(cols:Int, rows:Int, tc:TerrainCache, k:Int, seed:Long) = {
    val r = new Random(seed)
    val (prevS, _) = getAdjacent(tc, k)
    val ns = prevS.modBiome(k, r)
    val (terrain, spawns, nf) = ns.generate(cols, rows, k, r)
    val newStratum = ns.withNext(nf)
    (newStratum, terrain, spawns)
  }

  def create(cols:Int, rows:Int, r:Random):(TerrainCache, Cell, Direction, Seq[GlobalMessage]) = {
    val copy = new Random(r.nextInt())
    val stratum: Stratum = Stratum.createSurface(r)
    val (first, gs, nf) = stratum.generate(cols, rows, 0, copy)
    val newStratum = stratum.withNext(nf)
    val tf = newStratum.ts.toFactory(r)
    val tg = TerrainGenerator.unloaded
    val (nt, _) = Array2d.tabulate(cols, rows) { case (ij) =>
      tg.mkTile(newStratum.ts, tf, ij, 0, cols, rows, 0, r)
    }.unzip
    val unloaded = Terrain(0, tf, nt, (0,0), Direction.Down)
    val cache = TerrainCache(cols, rows, Map(0->((newStratum, first))), Map(), unloaded, Seq(), r)
    (cache, first.spawn, first.spawnFace, gs)
  }
}

case class TerrainCache(cols:Int, rows:Int, tMap:Map[Int,(Stratum, Terrain)], fs:Map[Int,Future[(Stratum,Terrain, Seq[GlobalMessage])]], dummy:Terrain, queuedSpawns:Seq[GlobalMessage], r:Random) {

  def getTileType(ij:Cell):TileType = {
    get(ij).tiles.get(toTerrainCoords(ij)).ttype
  }

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

  def mineralize(ij:Cell):TerrainCache = {
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


  def hit(ij:Cell, dmg:Damage, ttype:ToolType):(TerrainCache, Seq[GlobalMessage], HitResult) = {
    val (broke, dropped, result) = get(ij).hit(toTerrainCoords(ij), dmg, ttype)
    val index = getIndex(ij)
    (updateTerrain(index, broke), dropped, result)
  }

  private def insert(i:Int, s:Stratum, t:Terrain, ws:Seq[GlobalMessage]) = {
    copy(tMap=tMap.updated(i, (s, t)), queuedSpawns=queuedSpawns++ws)
  }

  private def addFuture(i:Int, f:Future[(Stratum, Terrain, Seq[GlobalMessage])]) = {
    copy(fs=fs.updated(i, f))
  }

  def update(ppos:Cell):(TerrainCache, Seq[GlobalMessage]) = {
    val newI = getIndex(ppos)
    val nCache = Seq(-1, 0, 1).map {_ + newI}.foldLeft(this) { case (ntc, index) =>
      if (ntc.tMap.contains(index)) {
        ntc
      } else {
        val (stratum, terrain, spawns) = TerrainCache.gen(cols, rows, ntc, index, r.nextLong())
        insert(index, stratum, terrain, spawns)
      }
    }
    val uCache = Seq(2).map {_ + newI}.foldLeft(nCache) { case (ntc, index) =>
      if (ntc.tMap.contains(index) || ntc.fs.contains(index)) {
        ntc
      } else {
        val f = new Future(() => TerrainCache.gen(cols, rows, ntc, index, r.nextLong()))
        ntc.addFuture(index, f)

      }
    }
    val (fCache, nfs) = TerrainCache.foldFutures(uCache)

    fCache.copy(queuedSpawns=Seq(), fs=nfs) @@ fCache.queuedSpawns
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

    val onScreen = ts.filter { ter => t.project(ter.getRect).intersects(t.screen)}
    Game.drawPerf.track("terrain") {
      onScreen.foldLeft(t) { _ <+< _.draw }
    }
  }

  def render(start:Int, end:Int, filename:String) {
    val span = end - start
    val poses = (start until end) map { i => (0, i*rows)}
    val cache = poses.foldLeft(this) { case (terr, ppos) =>
      val (ntc, _) = terr.update(ppos)
      ntc
    }
    val img = new BufferedImage(cols, rows*span, BufferedImage.TYPE_INT_RGB)
    (start until end).foreach { i =>
      Array2d.tabulate(cols, rows) { case p =>
        cache.getTileType(p +| i*rows)
      }.foreach { case (p, b) =>
        def cl(c:Color) = com.badlogic.gdx.graphics.Color.rgb888(c.toLibgdxColor)
        val color = b match {
          case Empty(_) => Color.White
          case Spike(_) => Color.Brown
          case Dirt => Color.Brown
          case Clay => Color.Red.mix(Color.Brown, 0.5)
          case Rock1 => Color.Grey
          case Rock2 => Color.DarkGrey
          case Rock3 => Color.DarkGreen
          case Mineral => Color.Purple
          case Shaft => Color.Black
        }
        val jcolor = cl(color)
        img.setRGB(p.x, p.y + (i - start)*rows, jcolor)
      }
    }
    val out = new File(filename)
    if (!ImageIO.write(img, "png", out)) {
      throw new RuntimeException()
    }
  }
}
