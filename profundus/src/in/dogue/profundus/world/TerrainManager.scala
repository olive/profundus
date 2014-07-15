package in.dogue.profundus.world

import in.dogue.profundus.entities._
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.entities.Massive
import in.dogue.profundus.particles.Particle
import scala.Some

class TerrainManager {
  private def updateShovel(tc:TerrainCache, pl:Player):(TerrainCache, Seq[MineralDrop], Player) = {
    pl.shovelPos match {
      case None => (tc, Seq(), pl)
      case Some(p) =>
        val (wHit, drops, damage) = tc.hit(p)
        (wHit, drops, pl.hitTool(damage))
    }
  }

  private def updateFacing(dir:Option[Direction], pl:Player):Player = {
    dir.map{d => pl.copy(face=d)}.getOrElse(pl)
  }

  private def updateClimb(tc:TerrainCache, pl:Player):Player = {
    import Direction._
    if (pl.isClimbing && Vector(Left, Right).contains(pl.face)) {
      if (       pl.face == Left
              && tc.isSolid(pl.pos --> Left)
              && !tc.isSolid(pl.pos --> Left --> Up)) {
        pl.move(pl.pos --> Left --> Up)
      } else if (pl.face == Right
              && tc.isSolid(pl.pos --> Right)
              && !tc.isSolid(pl.pos --> Right --> Up)) {
        pl.move(pl.pos --> Right --> Up)
      } else {
        pl
      }
    } else {
      pl
    }
  }

  private def processFall[T](tc:TerrainCache, p:Massive[T]) = {
    p.update(tc)
  }


  def update(ww:World, pp:Player):(World, Player, Seq[Particle[_]]) = {
    val (tryP, ps) = pp.update
    val (tc, drops, oldPl) = updateShovel(ww.cache, tryP)
    val added = ww.copy(cache=tc, es=ww.es.addDrops(drops))
    val dir = oldPl.getMove
    val pl = processFall(tc, updateFacing(dir, oldPl).toMassive)
    val specPos = dir.map {pl.pos --> _}.getOrElse(pl.pos)
    val newP = if (specPos == pl.pos || added.isSolid(specPos)) {
      pl
    } else {
      val newY = pl.fall match {
        case Floating => specPos._2
        case _ => pl.pos._2
      }
      pl.move((specPos._1, newY))
    }

    (added, updateClimb(tc, newP), ps)
  }
}
