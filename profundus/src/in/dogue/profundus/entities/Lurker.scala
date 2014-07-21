package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.world._
import scala.util.Random
import in.dogue.profundus.particles.{DeathParticle, Particle}
import in.dogue.profundus.entities.damagezones.{SingleTileZone, DamageZone}
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.world.WorldTile
import in.dogue.profundus.entities.damagezones.SingleTileZone
import in.dogue.profundus.Profundus
import in.dogue.profundus.entities.LurkerState.Wander

object Lurker {
  def create(ij:Cell) = {
    val tile = CP437.a.mkTile(Color.Black, Color.Yellow)
    Lurker(tile, Alive, Wander.create).toEntity(ij)
  }
}

object LurkerState {
  object Attack { def create(ppos:Cell) = Attack(ppos, 0) }
  case class Attack(ppos:Cell, t:Int) extends LurkerState {
    val attackFreq = 10
    def update = copy(t=t+1)
    override val isAttack = true
  }

  object Chase { def create(ppos:Cell) = Chase(ppos, 0) }
  case class Chase private (ppos:Cell, t:Int) extends LurkerState {
    def update(ppos:Cell) = copy(ppos=ppos, t=t+1)
  }
  object LostSight { def create(ppos:Cell) = LostSight(ppos, 0) }
  case class LostSight private (ppos:Cell, t:Int) extends LurkerState {
    def isDone = t > 120
  }
  object Wander { def create = Wander(0) }
  case class Wander private (t:Int) extends LurkerState {
    override val isWander = true
  }
}

sealed trait LurkerState {
  val isWander = false
  val isAttack = false
}



case class Lurker private (tile:Tile, live:LivingState, state:LurkerState) {
  import LurkerState._
  def move(ij:Cell, from:Direction, newTouching:Direction => Option[WorldTile]): Lurker = {
    if (newTouching(Direction.Down).exists {
      case WorldTile(Spike(_,_,dir,_)) if dir == Direction.Up => true
      case a => false

    }) {
      kill
    } else {
      this
    }
  }

  def getLiving = live

  def damage(dmg:Damage) = kill
  def kill = copy(live=Dead)
  private def updateWander(pos:Cell, w:Wander, c:TerrainCache, r:Random) = {
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
    (w.copy(t=w.t+1), pos, self, Seq())
  }

  private def updateChase(pos:Cell, c:Chase, cache:TerrainCache, ppos:Cell) = {
    val i = pos.x
    val j = pos.y
    val dd = ppos |-| pos
    val isAdjacent = math.abs(dd.x) + math.abs(dd.y) == 1
    val isIn = dd == ((0,0))
    val (self, newPos) = if (isIn) {
        val newPos = if (!cache.isSolid(pos --> Direction.Left)) {
          pos |- 1
        } else if (!cache.isSolid(pos --> Direction.Right)) {
          pos |+ 1
        } else {
          pos
        }
      (c.update(ppos), newPos)
    } else if (isAdjacent) {
      (Attack.create(ppos), pos)
    } else {
      val dx = math.signum(ppos.x - i)
      val dy = math.signum(ppos.y - j)
      val moved = pos |+| ((dx, dy))
      val newC = c.update(ppos)
      if (moved != ppos && newC.t % 7 == 0 && !cache.isSolid((i + dx, j + dy))) {
        (newC, moved)
      } else {
        (newC, pos)
      }
    }
    (self, newPos, this, Seq())
  }

  private def updateAttack(pos:Cell, a:Attack, ppos:Cell) = {
    val dd = ppos |-| pos
    val isAdjacent = math.abs(dd.x) + math.abs(dd.y) == 1
    val (newState, zone) = if (!isAdjacent) {
      (Chase.create(ppos), Seq())
    } else if (a.t > 0 && a.t % a.attackFreq == 0) {
      (a.copy(t=0), Seq(SingleTileZone(ppos, 75, DamageType.Lurker).toZone))
    } else {
      (a.update, Seq())
    }
    (newState, pos, this, zone)
  }

  private def updateLost(pos:Cell, l:LostSight) = {
    val state = if (l.isDone) {
      Wander.create
    } else {
      l.copy(t=l.t+1)
    }
    (state, pos, this, Seq())
  }


  private def updatePlayerAlive(pos:Cell, cache:TerrainCache, ppos:Cell, r:Random):(Lurker, Cell, Seq[GlobalSpawn]) = {
    import Profundus._
    val hasLos = cache.hasLineOfSight(pos, ppos)
    val ns = state match {
      case c@Chase(p, _) if !hasLos => LostSight.create(p)
      case c@Chase(p, _) if hasLos => c
      case a if hasLos && !a.isAttack => Chase.create(ppos)
      case a => a
    }
    val (newState, newPos, newSelf, attacks) = ns match {
      case a@Attack(p, t) => updateAttack(pos, a, ppos)
      case c@Chase(p, t) => updateChase(pos, c, cache, ppos)
      case l@LostSight(p, t) => updateLost(pos, l)
      case w@Wander(t) => updateWander(pos, w, cache, r)
    }
    (newSelf.copy(state = newState), newPos, Seq(attacks.gs))
  }

  private def updateAlive(pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random):(Lurker, Cell, Seq[GlobalSpawn]) = {
    pState match {
      case Alive => updatePlayerAlive(pos, cache, ppos, r)
      case Dead if !state.isWander => (copy(state=Wander.create), pos, Seq())
      case a => (this, pos, Seq())
    }
  }

  def update(pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random):(Lurker, Cell, Seq[GlobalSpawn], Seq[WorldSpawn]) = {
    live match {
      case Alive =>
        val (cre, newPos, glob) = updateAlive(pos, cache, ppos, pState, r)
        (cre, newPos, glob, Seq())
      case Dead => (this, pos, Seq(), Seq())
    }

  }
  def draw(pos:Cell)(tr:TileRenderer):TileRenderer = {
    tr <| (pos, tile)
  }

  def getDeathParticle(ij:Cell):Particle[_] = DeathParticle.create(ij, 60).toParticle

  def getLight(ij:Cell):Seq[LightSource] = Seq(LightSource.createCircle(ij, 0, 5, 0.5))

  def toEntity(ij:Cell):Entity[Lurker] = {
    Entity(ij, Grounded, _.update, _.move, _.damage, _.kill, _.getDeathParticle, _.getLight, _.getLiving, _.draw, this)
  }
}
