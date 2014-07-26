package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.{WorldTile, GlobalMessage, TerrainCache}
import scala.util.Random
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.particles.{DeathParticle, Particle}
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.lighting.LightSource

object StandardEntity {
  def create[T](up:T => (EntityId, Int, Int, Cell, TerrainCache, PlayerInfo, Random) => (T, Cell, Seq[GlobalMessage]),
                dr:T => Cell => TileRenderer => TileRenderer,
                onMove:T => (Direction => Option[WorldTile]) => T,
                self:T,
                light:LightSource,
                canFly:Boolean,
                selfType:Option[DamageType],
                health:Int,
                r:Random) = {
    val startT = r.nextInt.abs
    StandardEntity[T](up, dr, onMove, self, light, Alive, canFly, selfType, health, startT)
  }

  def apply[A](aup:A => (EntityId, Int, Int, Cell, TerrainCache, PlayerInfo, Random) => (A, Cell, Seq[GlobalMessage]),
               adr:A => Cell => TileRenderer => TileRenderer,
               aonMove:A => (Direction => Option[WorldTile]) => A,
               aself:A,
               alight:LightSource,
               alive:LivingState,
               acanFly:Boolean,
               aselfType:Option[DamageType],
               ahealth:Int,
               at:Int) = new StandardEntity {
    override type T = A
    override val up = aup
    override val dr = adr
    override val onMove = aonMove
    override val self = aself
    override val light = alight
    override val live = alive
    override val canFly = acanFly
    override val selfType = aselfType
    override val health = ahealth
    override val t = at
  }

  def NoMove[T](t:T)(f:Direction => Option[WorldTile]):T = t
}


trait StandardEntity {
  type T
  val up:T => (EntityId, Int, Int, Cell, TerrainCache, PlayerInfo, Random) => (T, Cell, Seq[GlobalMessage])
  val dr:T => Cell => TileRenderer => TileRenderer
  val onMove:T => (Direction => Option[WorldTile]) => T
  val self:T
  val light:LightSource
  val live:LivingState
  val canFly:Boolean
  val selfType:Option[DamageType]
  val health:Int
  val t:Int

  def copy(self:T=self, live:LivingState=live, health:Int=health, t:Int=t) = {
    StandardEntity(up, dr, onMove, self, light, live, canFly, selfType, health, t)
  }

  def update(id:EntityId, pos:Cell, cache:TerrainCache, pi:PlayerInfo, r:Random):(StandardEntity, Cell, Seq[GlobalMessage]) = {
    if (pi.live == Dead) {
      return (copy(t=t+1), pos, Seq())
    }
    val (newSelf, newPos, gs) = up(self)(id, health, t, pos, cache, pi, r)

    val newThis = copy(self=newSelf, t=t+1)
    val killed = if (health <= 0) {
      newThis.kill
    } else {
      newThis
    }

    (killed, newPos, gs)
  }

  def damage(dmg:Damage) = {
    if (selfType.exists{_ == dmg.source}) {
      this
    } else {
      copy(health=health.drop(dmg.amount))
    }
  }
  def getLive = live
  def move(ij:Cell, from:Direction, newTouching:Direction => Option[WorldTile])= {
    copy(self=onMove(self)(newTouching))
  }

  def kill = {
    copy(live= Dead)
  }
  def getDeathParticle(ij:Cell):Particle = DeathParticle.create(ij, 60).toParticle

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+< dr(self)(ij)
  }

  def getLight(ij:Cell) = Seq(light.copy(pos=ij))

  def toEntity(ij:Cell):Entity = {
    Entity.create[StandardEntity](ij, canFly.select(Grounded,Floating), _.update, _.move, _.damage, _.kill, _.getDeathParticle, _.getLight, _.getLive, _.draw, this)
  }
}
