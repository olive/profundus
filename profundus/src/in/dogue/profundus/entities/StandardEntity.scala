package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.{WorldTile, GlobalMessage, TerrainCache}
import scala.util.Random
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.particles.{DeathParticle, Particle}
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.audio.SoundManager

object StandardEntity {
  def create[T](up:T => (Int, Int, Cell, TerrainCache, PlayerInfo, Random) => (T, Cell, Seq[GlobalMessage]),
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

  def NoMove[T](t:T)(f:Direction => Option[WorldTile]):T = t
}


case class StandardEntity[T] private (up:T => (Int, Int, Cell, TerrainCache, PlayerInfo, Random) => (T, Cell, Seq[GlobalMessage]),
                                      dr:T => Cell => TileRenderer => TileRenderer,
                                      onMove:T => (Direction => Option[WorldTile]) => T,
                                      self:T,
                                      light:LightSource,
                                      live:LivingState,
                                      canFly:Boolean,
                                      selfType:Option[DamageType],
                                      health:Int,
                                      t:Int) {
  def update(pos:Cell, cache:TerrainCache, pi:PlayerInfo, r:Random):(StandardEntity[T], Cell, Seq[GlobalMessage]) = {
    if (pi.live == Dead) {
      return (copy(t=t+1), pos, Seq())
    }
    val (newSelf, newPos, gs) = up(self)(health, t, pos, cache, pi, r)

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

  def toEntity(ij:Cell):Entity[StandardEntity[T]] = {
    Entity(ij, canFly.select(Grounded,Floating), _.update, _.move, _.damage, _.kill, _.getDeathParticle, _.getLight, _.getLive, _.draw, this)
  }
}
