package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.world.{Spike, WorldTile, TerrainCache}
import scala.util.Random
import in.dogue.profundus.particles.{DeathParticle, Particle}
import in.dogue.profundus.entities.damagezones.{SingleTileZone, DamageZone}

object Creature {
  def create(i:Int, j:Int) = {
    val tile = CP437.a.mkTile(Color.Black, Color.Yellow)
    Creature(i, j, tile, Grounded, Alive, Wander.create)
  }
}

sealed trait CreatureState {
  val isWander = false
  val isAttack = false
}

object Attack { def create(ppos:Cell) = Attack(ppos, 0) }
case class Attack(ppos:Cell, t:Int) extends CreatureState {
  val attackFreq = 15
  def update = copy(t=t+1)
  override val isAttack = true
}

object Chase { def create(ppos:Cell) = Chase(ppos, 0) }
case class Chase private (ppos:Cell, t:Int) extends CreatureState {
  def update(ppos:Cell) = copy(ppos=ppos, t=t+1)
}
object LostSight { def create(ppos:Cell) = LostSight(ppos, 0) }
case class LostSight private (ppos:Cell, t:Int) extends CreatureState {
  def isDone = t > 120
}
object Wander { def create = Wander(0) }
case class Wander private (t:Int) extends CreatureState {
  override val isWander = true
}

case class Creature private (i:Int, j:Int, tile:Tile,
                             fall:FallState, live:LivingState, state:CreatureState) {
  def pos = (i, j)
  def move(ij:Cell, from:Direction, newTouching:Direction => Option[WorldTile]) = {
    val newCr = copy(i=ij.x, j=ij.y)
    if (newTouching(Direction.Down).exists {
      case WorldTile(Spike(_,_,dir,_)) if dir == Direction.Up => true
      case a => false

    }) {
      newCr.kill
    } else {
      newCr
    }
  }
  def setFall(f:FallState) = copy(fall=f)

  def damage(dmg:Int) = kill
  def kill = copy(live=Dead)
  private def updateWander(w:Wander, c:TerrainCache, r:Random) = {
    val self = if (w.t % 60 == 0) {
      val dir = Vector(Direction.Left, Direction.Right).randomR(r)
      if (!c.isSolid(pos --> dir)) {
        move(pos --> dir, dir, c.getTouching(pos --> dir))
      } else {
        this
      }
    } else {
      this
    }
    (w.copy(t=w.t+1), self, Seq())
  }

  private def updateChase(c:Chase, cache:TerrainCache, ppos:Cell) = {
    val dd = ppos |-| pos
    val isAdjacent = math.abs(dd.x) + math.abs(dd.y) == 1
    if (isAdjacent) {
      (Attack.create(ppos), this, Seq())
    } else {
      val dx = math.signum(ppos.x - i)
      val dy = math.signum(ppos.y - j)
      val moved = pos |+| ((dx, dy))
      val seen = copy(tile=CP437.b.mkTile(Color.Black, Color.Yellow))
      val newC = c.update(ppos)
      if (moved != ppos && newC.t % 7 == 0 && !cache.isSolid((i + dx, j + dy))) {
        (newC, copy(i=i+dx, j=j+dy), Seq())
      } else {
        (newC, seen, Seq())
      }
    }
  }

  private def updateAttack(a:Attack, ppos:Cell) = {
    val dd = ppos |-| pos
    val isAdjacent = math.abs(dd.x) + math.abs(dd.y) == 1
    val (newState, zone) = if (!isAdjacent) {
      (Chase.create(ppos), Seq())
    } else if (a.t % a.attackFreq == 0) {
      (a.copy(t=0), Seq(SingleTileZone(ppos, 50).toZone))
    } else {
      (a.update, Seq())
    }
    (newState, this, zone)
  }

  private def updateLost(l:LostSight) = {
    val state = if (l.isDone) {
      Wander.create
    } else {
      l.copy(t=l.t+1)
    }
    (state, this, Seq())
  }

  private def updatePlayerAlive(cache:TerrainCache, ppos:Cell, r:Random):(Creature, Seq[DamageZone[_]]) = {
    val hasLos = cache.hasLineOfSight((i, j), ppos)
    val ns = state match {
      case c@Chase(p, _) if !hasLos => LostSight.create(p)
      case c@Chase(p, _) if hasLos => c
      case a if hasLos && !a.isAttack => Chase.create(ppos)
      case a => a
    }
    val (newState, newSelf, attacks) = ns match {
      case a@Attack(p, t) => updateAttack(a, ppos)
      case c@Chase(p, t) => updateChase(c, cache, ppos)
      case l@LostSight(p, t) => updateLost(l)
      case w@Wander(t) => updateWander(w, cache, r)
    }
    (newSelf.copy(state = newState), attacks)
  }

  private def updateAlive(cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random):(Creature, Seq[DamageZone[_]]) = {
    pState match {
      case Alive => updatePlayerAlive(cache, ppos, r)
      case Dead if !state.isWander => (copy(state=Wander.create), Seq())
      case a => (this, Seq())
    }
  }

  def update(cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random):(Creature, Seq[DamageZone[_]]) = {
    live match {
      case Alive => updateAlive(cache, ppos, pState, r)
      case Dead => (this, Seq())
    }

  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <| (i, j, tile)
  }

  def getDeathParticle:Particle[_] = DeathParticle.create(i, j, 60).toParticle

  def toMassive:Massive[Creature] = Massive(_.pos, _.move, _.setFall, fall, this)
}
