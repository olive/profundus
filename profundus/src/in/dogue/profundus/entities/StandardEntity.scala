package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.{WorldTile, WorldSpawn, GlobalSpawn, TerrainCache}
import scala.util.Random
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.particles.{DeathParticle, Particle}
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.lighting.LightSource

case class StandardEntity[T](up:(Cell, TerrainCache, Cell, LivingState, Random) => (T, Cell, Seq[GlobalSpawn], Seq[WorldSpawn]),
                             dr:Cell => TileRenderer => TileRenderer,
                             light:LightSource, live:LivingState, health:Int, t:Int) {
  def update(pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random):(T, Cell, Seq[GlobalSpawn], Seq[WorldSpawn]) = {

    //up(pos, cache, ppos, pState, r).map1((value: T) => value.copy(t = t + 1))
    ???
  }

  def damage(dmg:Damage) = copy(health=health.drop(dmg.amount))
  def getLive = live
  def move(ij:Cell, from:Direction, newTouching:Direction => Option[WorldTile]): T = {
    ???
  }

  def kill = copy(live=Dead)
  def getDeathParticle(ij:Cell):Particle = DeathParticle.create(ij, 60).toParticle

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+< dr(ij)
  }

  def getLight(ij:Cell) = Seq(light.copy(pos=ij))

  def toEntity(ij:Cell):Entity[StandardEntity[_]] = {
    ???
    //Entity(ij, Floating, _.update, _.move, _.damage, _.kill, _.getDeathParticle, _.getLight, _.getLive, _.draw, this)
  }
}
