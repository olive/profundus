package in.dogue.profundus.world

import in.dogue.profundus.entities._
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.entities.Massive
import in.dogue.profundus.particles.Particle
import scala.Some
import in.dogue.profundus.entities.pickups.Pickup

class TerrainManager {
  private def updateShovel(tc:TerrainCache, pl:Player):(TerrainCache, Seq[Pickup[_]], Player) = {
    pl.toolPos match {
      case Some(p) =>
        val (wHit, drops, damage, broken) = tc.hit(p, pl.inv.tool.`type`.digDamage/*fixme*/, pl.inv.tool.`type`)
        (wHit, drops, pl.hitTool(damage, broken))
      case None => (tc, Seq(), pl)
    }
  }

  private def updateFacing(dir:Direction, pl:Player):Player = {
    pl.setFacing(dir)
  }

  private def updateClimb(tc:TerrainCache, pl:Player):Player = {
    import Direction._
    if (pl.ctrl.isClimbing && Vector(Left, Right).contains(pl.face)) {
      if (       pl.face == Left
              && tc.isSolid(pl.pos --> Left)
              && !tc.isSolid(pl.pos --> Left --> Up)) {
        val newPos = pl.pos --> Left --> Up
        pl.move(newPos, Left, tc.getTouching(newPos))
      } else if (pl.face == Right
              && tc.isSolid(pl.pos --> Right)
              && !tc.isSolid(pl.pos --> Up --> Right)) {
        val newPos = pl.pos --> Right --> Up
        pl.move(newPos, Right, tc.getTouching(newPos))
      } else {
        if (pl.hasLongArms) {
          updateLongClimb(tc, pl)
        } else {
          pl

        }
      }
    } else {
      pl
    }
  }

  private def updateLongClimb(tc:TerrainCache, pl:Player):Player = {
    import Direction._
    if (       pl.face == Left
            && tc.isSolid(pl.pos --> Left)
            && tc.isSolid(pl.pos --> Left --> Up)
            && !tc.isSolid(pl.pos --> Left --> Up --> Up)) {
      val newPos = pl.pos --> Left --> Up --> Up
      pl.move(newPos, Left, tc.getTouching(newPos))
    } else if (pl.face == Right
            && tc.isSolid(pl.pos --> Right)
            && tc.isSolid(pl.pos --> Right --> Up)
            && !tc.isSolid(pl.pos --> Right --> Up --> Up)) {
      val newPos = pl.pos --> Right --> Up --> Up
      pl.move(newPos, Right, tc.getTouching(newPos))

    } else {
      pl
    }
  }

  private def processFall[T](tc:TerrainCache, p:Massive[T]): T = {
    p.update(tc)
  }


  def update(tcache:TerrainCache, pp:Player):(TerrainCache, Player, Seq[Pickup[_]], Seq[Particle[_]]) = {
    val (tryP, ps) = pp.update

    val (tc, drops, oldPl) = updateShovel(tcache, tryP)
    val (movePl, dir) = oldPl.getMove
    val pl = processFall(tc, updateFacing(movePl.instDir, movePl).toMassive)
    val specPos = dir.map {pl.pos --> _}.getOrElse(pl.pos)
    val newP = if (specPos == pl.pos || tc.isSolid(specPos)) {
      pl
    } else {
      val newY = pl.fall match {
        case Floating => specPos.y
        case _ => pl.pos.y
      }
      val newPos = (specPos.x, newY)
      val dx = pp.pos.x - specPos.x
      val dy = 0
      val face = pl.chooseFace(dx, dy)
      pl.move(newPos, face, tc.getTouching(newPos))
    }
    val plResult = updateClimb(tc, newP)
    (tc, plResult, drops, ps)
  }
}
