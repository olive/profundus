package in.dogue.profundus.world

import in.dogue.profundus.entities.{Floating, Massive, Falling, Player}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Direction

class TerrainManager {
  private def updateShovel(w:World, pl:Player):(World, Player) = {
    pl.shovelPos match {
      case None => (w, pl)
      case Some(p) =>
        val (wHit, damage) = w.hit(p)
        (wHit, pl.hitTool(damage))
    }
  }

  private def updateFacing(dir:Option[Direction], pl:Player):Player = {
    dir.map{d => pl.copy(face=d)}.getOrElse(pl)
  }

  private def updateClimb(w:World, pl:Player):Player = {
    import Direction._
    if (pl.isClimbing && Vector(Left, Right).contains(pl.face)) {
      if (       pl.face == Left
              && w.isSolid(pl.pos --> Left)
              && !w.isSolid(pl.pos --> Left --> Up)) {
        pl.move(pl.pos --> Left --> Up)
      } else if (pl.face == Right
              && w.isSolid(pl.pos --> Right)
              && !w.isSolid(pl.pos --> Right --> Up)) {
        pl.move(pl.pos --> Right --> Up)
      } else {
        pl
      }
    } else {
      pl
    }
  }

  private def processFall[T](w:World, p:Massive[T]) = {
    p.update(w)
  }


  def update(ww:World, pp:Player):(World, Player) = {
    val (w, oldPl) = updateShovel(ww, pp.update)
    val dir = oldPl.getMove
    val pl = processFall(w, updateFacing(dir, oldPl).toMassive)
    val specPos = dir.map {pl.pos --> _}.getOrElse(pl.pos)
    val newP = if (specPos == pl.pos || w.isSolid(specPos)) {
      pl
    } else {
      val newY = pl.fall match {
        case Floating => specPos._2
        case _ => pl.pos._2
      }
      pl.move((specPos._1, newY))
    }

    (w, updateClimb(w, newP))
  }
}
