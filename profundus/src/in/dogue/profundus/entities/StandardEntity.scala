package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.{WorldTile, WorldSpawn, TerrainCache}
import scala.util.Random
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.particles.{DeathParticle, Particle}
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.audio.SoundManager

object StandardEntity {
  def create[T](up:T => (Int, Int, Cell, TerrainCache, Cell, LivingState, Random) => (T, Cell, Seq[WorldSpawn]),
                dr:T => Cell => TileRenderer => TileRenderer,
                self:T,
                light:LightSource,
                canFly:Boolean,
                selfType:Option[DamageType],
                health:Int,
                r:Random) = {
    val startT = r.nextInt.abs
    StandardEntity[T](up, dr, self, light, Alive, canFly, selfType, health, startT)
  }
}


case class StandardEntity[T] private (up:T => (Int, Int, Cell, TerrainCache, Cell, LivingState, Random) => (T, Cell, Seq[WorldSpawn]),
                                      dr:T => Cell => TileRenderer => TileRenderer,
                                      self:T,
                                      light:LightSource,
                                      live:LivingState,
                                      canFly:Boolean,
                                      selfType:Option[DamageType],
                                      health:Int,
                                      t:Int) {
  def update(pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random):(StandardEntity[T], Cell, Seq[WorldSpawn]) = {
    if (pState == Dead) {
      return (copy(t=t+1), pos, Seq())
    }
    val (newSelf, newPos, gs) = up(self)(health, t, pos, cache, ppos, pState, r)
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
    this
  }

  def kill = {
    if (live == Alive) {
      SoundManager.enedie.play()
    }
    copy(live=Dead)
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
