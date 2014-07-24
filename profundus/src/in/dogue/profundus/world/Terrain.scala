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



/* DONT FORGET TO ADD y TO SPAWN VALUES! */
case class Terrain(y:Int, ts:TerrainScheme, tiles:Array2d[WorldTile], spawn:Cell, spawnFace:Direction) {

  def getRect = Recti(0, y, tiles.cols, tiles.rows)

  def isSolid(s:Cell):Boolean = {
    val t = tiles.getOption(s)
    !t.exists{_.state.isWalkable}
  }

  def isBackgroundSolid(s:Cell):Boolean = {
    val t = tiles.getOption(s)
    t.exists{_.state.bgSolid}
  }

  def isRock(s:Cell):Boolean = {
    val t = tiles.getOption(s)
    t.exists{_.isRock}
  }

  def mineralize(s:Cell, seed:Int):Terrain = {
    val r = new Random(seed)
    val t = WorldTile(ts.makeMineral(r))
    val newTiles = tiles.updated(s, t)
    copy(tiles=newTiles)
  }


  def hit(ij:Cell, dmg:Int, ttype:ToolType):(Terrain, Seq[GlobalSpawn], Int, Boolean) = {
    val to = tiles.getOption(ij)
    if (!to.exists{_.canBreakBy(ttype.breakable)}) {
      (this, Seq(), 0, false)
    } else {
      val t = to.get//fixme
      val (newState, drops, damage, broken) = t.state.hit(ij +| y, dmg)
      val newT = copy(tiles=tiles.update(ij, _.copy(state=newState)))
      (newT, drops, damage, broken)
    }

  }

  def draw(tr:TileRenderer):TileRenderer = {
    tiles.foldLeft(tr) { case (r, (p, t)) =>
      r <+ (p +| y, t.tile)
    }
  }

}
