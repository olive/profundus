package in.dogue.profundus.world

import in.dogue.profundus.entities.{Massive, Falling, Player}
import in.dogue.antiqua.Implicits
import Implicits._
import in.dogue.antiqua.data.Direction

class TerrainManager {
  private def updateShovel(w:World, pl:Player):(World, Player) = {
    val world = pl.shovelPos.map { p =>
      w.break(p)
    }.getOrElse(w)
    (world, pl)
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

  private def processFall2[T](w:World, p:Massive[T]) = {
    p.update(w)
  }

  private def processFall(w:World, p:Player) = {
    processFall2(w, p.toMassive)
    /*val grounded = w.isGrounded(p.pos)
    p.fall match {
      case f@Falling(t, tiles) if !grounded =>
        val newT = (t + 1) % f.fallTime
        val newTiles = tiles + (newT == 0).select(0, 1)
        val newPos = (newT==0).select(p.pos, p.pos --> Direction.Down)
        p.move(newPos).copy(fall = Falling(newT, newTiles))
      case _ if !grounded => p.copy(fall = Falling(0,0))
      case _ => p.land
    }*/

  }

  def update(ww:World, pp:Player):(World, Player) = {
    val (w, oldPl) = updateShovel(ww, pp.update)
    val dir = oldPl.getMove
    val pl = processFall(w, updateFacing(dir, oldPl))
    val specPos = dir.map {pl.pos --> _}.getOrElse(pl.pos)
    val newP = if (specPos == pl.pos || w.isSolid(specPos)) {
      pl
    } else {
      pl.move((specPos._1, pl.pos._2))
    }

    (w, updateClimb(w, newP))
  }
}
