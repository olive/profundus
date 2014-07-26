package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.world.{Unloadable, GlobalMessage, TerrainCache, WorldTile}
import in.dogue.antiqua.Antiqua.Cell
import scala.util.Random
import in.dogue.profundus.particles.{Particle, DeathParticle}
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.audio.SoundManager
import in.dogue.profundus.Game
import in.dogue.profundus.entities.pickups.PickupId
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._

object EntityId {
  var id:BigInt = 0
  def create:EntityId = {
    val result = EntityId(id)
    synchronized(this) {
      id += 1
      0
    }
    result
  }
}
case class EntityId private (id:BigInt)

case class EntityArgs(id:EntityId, pos:Cell, tc:TerrainCache, pi:PlayerInfo, r:Random) {
  val dd = pi.pos |-| pos
  def distance2 = dd.mag2
  @inline def ppos = pi.pos
  def isAdjacent = {
    math.abs(dd.x) + math.abs(dd.y) == 1
  }

  def isOnTop = {
    math.abs(dd.x) + math.abs(dd.y) == 0
  }

  def hasLos = {
    tc.hasLineOfSight(pos, pi.pos)
  }

  def moveRandom:Cell = {
    val move = Direction.All.find { d =>
      !tc.isSolid(pos --> d)
    }
    move.map{pos --> _}.getOrElse(pos)
  }

  def moveHorizontalRandom:Cell = {
    val move = Vector(Direction.Left, Direction.Right).find { d =>
      !tc.isSolid(pos --> d)
    }
    move.map{pos --> _}.getOrElse(pos)
  }

  def toward = dd.signum
}

object Entity {
  var lastPlayed = 0
  private def apply[A](aij:Cell,
                       afall:FallState,
                       aup: A => EntityArgs => (A, Cell, Seq[GlobalMessage]),
                       amv: A => (Cell, Direction, (Direction => Option[WorldTile])) => A,
                       admg: A => Damage => A,
                       adoKill: A => A,
                       adeathPart:A => Cell => Particle,
                       alight: A => Cell => Seq[LightSource],
                       alive:A => LivingState,
                       adr:A => Cell => TileRenderer => TileRenderer,
                       aself:A,
                       aid:EntityId) = new Entity {

    override type T = A
    override protected val id = aid
    override val deathPart = adeathPart
    override val self = aself
    override val up = aup
    override val dmg = admg
    override val fall = afall
    override val live = alive
    override val mv = amv
    override val light = alight
    override val doKill = adoKill
    override val dr = adr
    override val ij = aij
  }

  def create[A](ij:Cell,
                fall:FallState,
                up: A => EntityArgs => (A, Cell, Seq[GlobalMessage]),
                mv: A => (Cell, Direction, (Direction => Option[WorldTile])) => A,
                dmg: A => Damage => A,
                doKill: A => A,
                deathPart:A => Cell => Particle,
                light: A => Cell => Seq[LightSource],
                live:A => LivingState,
                dr:A => Cell => TileRenderer => TileRenderer,
                self:A) = {
    Entity(ij, fall, up, mv, dmg, doKill, deathPart, light, live, dr, self, EntityId.create)
  }

}
trait Entity {
  type T
  val ij:Cell
  val fall:FallState
  val up: T => EntityArgs => (T, Cell, Seq[GlobalMessage])
  val mv: T => (Cell, Direction, (Direction => Option[WorldTile])) => T
  val dmg: T => Damage => T
  val doKill: T => T
  val deathPart:T => Cell => Particle
  val light: T => Cell => Seq[LightSource]
  val live:T => LivingState
  val dr:T => Cell => TileRenderer => TileRenderer
  val self:T
  protected val id:EntityId


  def copy(self:T=self, ij:Cell=ij, fall:FallState=fall) = {
    Entity(ij, fall, up, mv, dmg, doKill, deathPart, light, live, dr, self, id)
  }

  def isId(id:EntityId) = this.id == id
  def pos = ij
  def update(tc:TerrainCache, pi:PlayerInfo, r:Random): (Entity, Seq[GlobalMessage]) = {
    val (t, newPos, glob) = up(self)(EntityArgs(id, ij, tc, pi, r))
    (copy(self=t, ij=newPos), glob)
  }

  def setFall(f:FallState): Entity = copy(fall=f)
  def getFall = fall
  def move(to:Cell, dir:Direction, newSolid:(Direction => Option[WorldTile])) = {
    copy(ij=to, self=mv(self)(to, dir, newSolid))
  }

  def damage(d:Damage) = {
    if (d.amount > 0 && Game.t - Entity.lastPlayed > 7) {
      SoundManager.enehit.play(pos)
      Entity.lastPlayed = Game.t
    }
    copy(self=dmg(self)(d))
  }

  def kill = copy(self=doKill(self))

  def getLiving = live(self)

  def draw(tr:TileRenderer):TileRenderer = tr <+< dr(self)(pos)
  def getLight = light(self)(pos)
  def getDeathParticle = deathPart(self)(pos)
  def toMassive:Massive[Entity] = Massive[Entity](_.pos, _.move, (f:Entity) => f.setFall, getFall, this)
  def toUnloadable = Unloadable.fromPos[Entity](this, _.pos)
}
