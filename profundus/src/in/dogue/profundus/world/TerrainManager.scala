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
      pl.move((specPos._1, pl.pos._2))
    }

    (w, updateClimb(w, newP))
  }
}
