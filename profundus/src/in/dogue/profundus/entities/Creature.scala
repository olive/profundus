package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.world.TerrainCache
import scala.util.Random

object Creature {
  def create(i:Int, j:Int) = {
    val tile = CP437.a.mkTile(Color.Black, Color.Yellow)
    Creature(i, j, tile, Grounded, Alive, Wander.create)
  }
}

sealed trait CreatureState
object Chase { def create(ppos:(Int,Int)) = Chase(ppos, 0) }
case class Chase private (ppos:(Int,Int), t:Int) extends CreatureState {
  def update(ppos:(Int,Int)) = copy(ppos=ppos, t=t+1)
}
object LostSight { def create(ppos:(Int,Int)) = LostSight(ppos, 0) }
case class LostSight private (ppos:(Int,Int), t:Int) extends CreatureState {
  def isDone = t > 120
}
object Wander { def create = Wander(0) }
case class Wander private (t:Int) extends CreatureState

case class Creature private (i:Int, j:Int, tile:Tile,
                             fall:FallState, live:LivingState, state:CreatureState) {
  def pos = (i, j)
  def move(ij:(Int,Int)) = copy(i=ij.x, j=ij.y)
  def setFall(f:FallState) = copy(fall=f)


  private def updateWander(w:Wander, c:TerrainCache, r:Random) = {
    val self = if (w.t % 60 == 0) {
      val dir = Vector(Direction.Left, Direction.Right).randomR(r)
      if (!c.isSolid(pos --> dir)) {
        move(pos --> dir)
      } else {
        this
      }
    } else {
      this
    }
    (w.copy(t=w.t+1), self)
  }

  private def updateChase(c:Chase, cache:TerrainCache, ppos:(Int,Int)) = {
    val dd = ppos |-| pos
    val isAdjacent = math.abs(dd.x + dd.y) == 1
    val dx = math.signum(ppos.x - i)
    val dy = math.signum(ppos.y - j)
    val seen = copy(tile=CP437.b.mkTile(Color.Black, Color.Yellow))
    val newC = c.update(ppos)
    if (!isAdjacent && newC.t % 7 == 0 && !cache.isSolid((i + dx, j + dy))) {
      (newC, copy(i=i+dx, j=j+dy))
    } else {
      (newC, seen)
    }
  }

  private def updateLost(l:LostSight) = {
    if (l.isDone) {
      (Wander.create, this)
    } else {
      (l.copy(t=l.t+1), this)
    }
  }

  private def updateAlive(cache:TerrainCache, ppos:(Int,Int), r:Random) = {
    val hasLos = cache.hasLineOfSight((i,j), ppos)
    val ns =  state match {
      case c@Chase(p,_) if !hasLos => LostSight.create(p)
      case c@Chase(p,_) if hasLos => c
      case a if hasLos => Chase.create(ppos)
      case a => a
    }
    val (newState, newSelf) = ns match {
      case c@Chase(p, t) => updateChase(c, cache, ppos)
      case l@LostSight(p, t) => updateLost(l)
      case w@Wander(t) => updateWander(w, cache, r)
    }
    newSelf.copy(state=newState)
  }

  def update(cache:TerrainCache, ppos:(Int,Int), r:Random) = {
    live match {
      case Alive => updateAlive(cache, ppos, r)
      case Dead => this
    }

  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <| (i, j, tile)
  }
  def toMassive:Massive[Creature] = Massive(_.pos, _.move, _.setFall, fall, this)
}
