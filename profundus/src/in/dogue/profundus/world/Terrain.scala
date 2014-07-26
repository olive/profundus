package in.dogue.profundus.world

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.data.{Direction,Array2d}
import scala.util.Random
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.entities.{Damage, ToolType}
import in.dogue.profundus.Profundus
import Profundus._
object Terrain {
  def merge(tiles:Array2d[WorldTile], deps:Array2d[Option[Cell]]):Array2d[WorldTile] = {
    val depMap = deps.flatten.map { case (cell, optCell) =>
      optCell map { dest => (cell, dest)}
    }.flatten.toMap
    val myMap = collection.mutable.Map[Cell,WorldTile]()
    for (i <- 0 until tiles.cols; j <- 0 until tiles.rows) yield {
      val p = (i,j)
      depMap.get(p) match {
        case Some(dep) =>
          val tt: WorldTile = myMap.get(dep) match {
            case Some(t) => t.addDep(p)
            case None => tiles.getOption(dep).map { _.addDep(p)}.getOrElse(tiles.get(p))
          }
          myMap(dep) = tt
        case None =>
          if (!myMap.contains(p)) {
            myMap(p) = tiles.get(p)
          }
      }
    }
    Array2d.tabulate(tiles.cols, tiles.rows) { case p =>
      myMap.get(p).getOrElse(tiles.get(p))
    }
  }
}

/* DONT FORGET TO ADD y TO SPAWN VALUES! */
case class Terrain(y:Int, tf:WorldTileFactory, tiles:Array2d[WorldTile], spawn:Cell, spawnFace:Direction) {

  def getRect = Recti(0, y, tiles.cols, tiles.rows)

  private def updated(ij:Cell, wt:WorldTile) = {
    copy(tiles = tiles.updated(ij, wt))
  }

  def isSolid(s:Cell):Boolean = {
    val t = tiles.getOption(s)
    !t.exists{_.isWalkable}
  }

  def isBackgroundSolid(s:Cell):Boolean = {
    val t = tiles.getOption(s)
    t.exists{_.isBgSolid}
  }

  def isRock(s:Cell):Boolean = {
    val t = tiles.getOption(s)
    t.exists{_.isRock}
  }

  def mineralize(s:Cell):Terrain = {

    val t = tf.mkMineral._1//fixme
    val newTiles = tiles.updated(s, t)
    copy(tiles=newTiles)
  }


  def hit(ij:Cell, dmg:Damage, ttype:ToolType):(Terrain, Seq[WorldSpawn], HitResult) = {
    val to = tiles.getOption(ij)
    if (!to.exists{_.canBreakBy(ttype.breakable)}) {
      (this, Seq(), HitResult(false, 0, 0))
    } else {
      val t = to.get//fixme
      val (newTile, drops, result) = t.hit(tf, ij +| y, dmg)
      val newT = copy(tiles=tiles.updated(ij, newTile))
      if (result.broken) {
        val (newTiles, gs) = fold2[Terrain, Seq[WorldSpawn], Cell](newT, t.dependents) { case (dep, terrain: Terrain) =>
          val t = terrain.tiles.get(dep)
          val (mod, gs) = t.notifyTile(tf, dep)
          (terrain.updated(dep, mod), gs)
        }
        (newTiles, drops ++ gs, result)
      } else {
        (newT, drops, result)

      }
    }

  }

  def draw(tr:TileRenderer):TileRenderer = {
    tiles.foldLeft(tr) { case (r, (p, t)) =>
      r <+ (p +| y, t.tile)
    }
  }

}
