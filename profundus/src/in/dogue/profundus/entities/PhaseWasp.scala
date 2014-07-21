package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Animation, TileFactory}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.world.{WorldSpawn, GlobalSpawn, TerrainCache, WorldTile}
import in.dogue.profundus.particles.{DeathParticle, Particle}

object PhaseWasp {
  def create(ij:Cell, r:Random) = {
    val tf = TileFactory(Color.Black, Color.Red.dim(2))
    val rightWing = Animation.create(Vector(
      (15, tf(CP437.¬)),
      (15, tf(CP437.^))

    ))
    val leftWing = Animation.create(Vector(
      (15, tf(CP437.⌐)),
      (15, tf(CP437.^))

    ))
    val body = Animation.singleton(tf(CP437.◦))
    val anim = Vector(
      ((-1,0), leftWing),
      ((0,0), body),
      ((1,0), rightWing)
    )
    val light = LightSource.createCircle(ij, 0, 3, 0.2)
    PhaseWasp(anim, light, Alive, 12, math.abs(r.nextInt)).toEntity(ij)
  }
}

case class PhaseWasp(a:AnimationGroup, light:LightSource, live:LivingState, health:Int, t:Int) {
  def damage(dmg:Damage) = copy(health=health.drop(dmg.amount))
  def getLive = live
  def move(ij:Cell, from:Direction, newTouching:Direction => Option[WorldTile]) = {
    this
  }

  def kill = copy(live=Dead)

  def update(pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random) = {


    (this, pos, Seq(), Seq())
  }

  def getDeathParticle(ij:Cell):Particle[_] = DeathParticle.create(ij, 60).toParticle

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <++< a.map{ case (c, anim) => anim.drawFg(c.x + ij.x, c.y + ij.y) _}
  }

  def getLight(ij:Cell) = Seq(light.copy(pos=ij))

  def toEntity(ij:Cell):Entity[PhaseWasp] = {
    Entity(ij, Floating, _.update, _.move, _.damage, _.kill, _.getDeathParticle, _.getLight, _.getLive, _.draw, this)
  }
}
