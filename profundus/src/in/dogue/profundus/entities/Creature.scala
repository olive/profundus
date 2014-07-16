package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.world.TerrainCache
import scala.util.Random
import in.dogue.profundus.particles.{DeathParticle, Particle}

object Creature {
  def create(i:Int, j:Int) = {
    val tile = CP437.a.mkTile(Color.Black, Color.Yellow)
    Creature(i, j, tile, Grounded, Alive, Wander.create)
  }
}

sealed trait CreatureState {
  val isWander = false
}
object Chase { def create(ppos:(Int,Int)) = Chase(ppos, 0) }
case class Chase private (ppos:(Int,Int), t:Int) extends CreatureState {
  def update(ppos:(Int,Int)) = copy(ppos=ppos, t=t+1)
}
object LostSight { def create(ppos:(Int,Int)) = LostSight(ppos, 0) }
case class LostSight private (ppos:(Int,Int), t:Int) extends CreatureState {
  def isDone = t > 120
}
object Wander { def create = Wander(0) }
case class Wander private (t:Int) extends CreatureState {
  override val isWander = true
}

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
    (w.copy(t=w.t+1), self, Seq())
  }

  private def updateChase(c:Chase, cache:TerrainCache, ppos:(Int,Int)) = {
    val dd = ppos |-| pos
    val isAdjacent = math.abs(dd.x) + math.abs(dd.y) == 1
    val dx = math.signum(ppos.x - i)
    val dy = math.signum(ppos.y - j)
    val seen = copy(tile=CP437.b.mkTile(Color.Black, Color.Yellow))
    val newC = c.update(ppos)
    if (isAdjacent) {
      (newC, seen, Seq(SingleTileKillZone(ppos).toKillZone))
    } else if (newC.t % 7 == 0 && !cache.isSolid((i + dx, j + dy))) {
      (newC, copy(i=i+dx, j=j+dy), Seq())
    } else {
      (newC, seen, Seq())
    }
  }

  private def updateLost(l:LostSight) = {
    val state = if (l.isDone) {
      Wander.create
    } else {
      l.copy(t=l.t+1)
    }
    (state, this, Seq())
  }

  private def updatePlayerAlive(cache:TerrainCache, ppos:(Int,Int), r:Random):(Creature, Seq[KillZone[_]]) = {
    val hasLos = cache.hasLineOfSight((i, j), ppos)
    val ns = state match {
      case c@Chase(p, _) if !hasLos => LostSight.create(p)
      case c@Chase(p, _) if hasLos => c
      case a if hasLos => Chase.create(ppos)
      case a => a
    }
    val (newState, newSelf, attacks) = ns match {
      case c@Chase(p, t) => updateChase(c, cache, ppos)
      case l@LostSight(p, t) => updateLost(l)
      case w@Wander(t) => updateWander(w, cache, r)
    }
    (newSelf.copy(state = newState), attacks)
  }
  private def updateAlive(cache:TerrainCache, ppos:(Int,Int), pState:LivingState, r:Random):(Creature, Seq[KillZone[_]]) = {
    pState match {
      case Alive => updatePlayerAlive(cache, ppos, r)
      case Dead if !state.isWander => (copy(state=Wander.create), Seq())
      case a => (this, Seq())
    }
  }

  def update(cache:TerrainCache, ppos:(Int,Int), pState:LivingState, r:Random):(Creature, Seq[KillZone[_]]) = {
    live match {
      case Alive => updateAlive(cache, ppos, pState, r)
      case Dead => (this, Seq())
    }

  }

  def getDeathParticle:Particle[_] = DeathParticle.create(i, j, 60).toParticle

  def draw(tr:TileRenderer):TileRenderer = {
    tr <| (i, j, tile)
  }
  def toMassive:Massive[Creature] = Massive(_.pos, _.move, _.setFall, fall, this)
}