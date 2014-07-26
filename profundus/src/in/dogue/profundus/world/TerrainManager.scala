package in.dogue.profundus.world

import in.dogue.profundus.entities._
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.entities.Massive
import in.dogue.profundus.particles.Particle
import scala.Some
import in.dogue.profundus.entities.pickups.Pickup
import in.dogue.profundus.Profundus

class TerrainManager {
  private def updateShovel(tc:TerrainCache, pl:Player):(TerrainCache, Seq[GlobalMessage], Player) = {

    pl.toolPos match {
      case Some(p) =>
        val dmg = Damage(pl.inv.tool.`type`.digDamage/*fixme*/, DamageType.Tool)
        val (wHit, drops, result) = tc.hit(p, dmg, pl.inv.tool.`type`)
        (wHit, drops, pl.hitTool(result))
      case None => (tc, Seq(), pl)
    }
  }
  private val climbTime = 5
  private def updateFacing(dir:Direction, pl:Player):Player = {
    pl.setFacing(dir)
  }

  private def updateClimb(tc:TerrainCache, pl:Player):Player = {
    import Direction._
    if (pl.ctrl.isClimbing && Vector(Left, Right).contains(pl.face)) {
      if (       pl.face == Left
              && tc.isSolid(pl.pos --> Left)
              && !tc.isSolid(pl.pos --> Left --> Up)) {
        pl.setForce(Force.mkClimbForce(Vector(Up, Left), climbTime))
      } else if (pl.face == Right
              && tc.isSolid(pl.pos --> Right)
              && !tc.isSolid(pl.pos --> Up --> Right)) {
        pl.setForce(Force.mkClimbForce(Vector(Up, Right), climbTime))
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
      pl.setForce(Force.mkClimbForce(Vector(Up, Up, Left), climbTime))
    } else if (pl.face == Right
            && tc.isSolid(pl.pos --> Right)
            && tc.isSolid(pl.pos --> Right --> Up)
            && !tc.isSolid(pl.pos --> Right --> Up --> Up)) {
      pl.setForce(Force.mkClimbForce(Vector(Up, Up, Right), climbTime))

    } else {
      pl
    }
  }

  private def processFall[T](tc:TerrainCache, p:Massive[T]): T = {
    p.update(tc)
  }

  private def processForces(tc:TerrainCache, p:Player) = {
    p.processForces(tc)
  }

  def update(tcache:TerrainCache, pp:Player):(TerrainCache, Player, Seq[GlobalMessage]) = {
    import Profundus._
    val (tryP, gs) = pp.update
    val (dropP, tool) = tryP.updateDropTool(tryP, tcache)
    val (tc, drops, oldPl) = updateShovel(tcache, dropP)
    val (movePl, dir) = oldPl.getMove
    val pl1 = processFall(tc, updateFacing(movePl.instDir, movePl).toMassive)
    val pl = processForces(tc, pl1)
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
    (tc, plResult, gs ++ drops ++ tool.gss)
  }
}
