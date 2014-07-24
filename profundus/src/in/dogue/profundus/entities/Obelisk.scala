package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua.{TileGroup, Cell}
import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import in.dogue.profundus.entities.damagezones.ExplosionZone
import in.dogue.profundus.world._
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.particles.{RingParticle, DeathParticle, Particle, ExplosionParticle}
import in.dogue.profundus.entities.pickups.{ToolPickup, ItemPickup}
import in.dogue.profundus.particles.Particle
import in.dogue.profundus.world.WorldTile
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.audio.SoundManager

object Obelisk {
  val attackTime = 10*60
  def create(ij:Cell, r:Random) = {
    val tg = Tile.makeGroup(Vector(
      (0, 0, CP437.ç.toCode, Color.Black, Color.White),
      (0, 1, CP437.↑.toCode, Color.Black, Color.White)
    ))
    val light = LightSource.createCircle(ij, 3, 4, 0.5)
    val obelisk = Obelisk(tg, 0)
    StandardEntity.create[Obelisk](_.update, _.draw, obelisk, light, true, DamageType.Obelisk.some, 50, r).toEntity(ij)

  }
}
case class Obelisk private (tg:TileGroup, mixAmt:Double) {
  import Profundus._

  def update(health:Int, t:Int, pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random):(Obelisk, Cell, Seq[WorldSpawn]) = {

    val spawns = if (t > 0 && t % Obelisk.attackTime == 0) {
      SoundManager.pop.play()
      val ps = RingParticle.create(pos, 8, 3).toParticle
      val ex = ExplosionZone.create(pos, 8, 3, DamageType.Obelisk).toZone
      Seq(ps).gss ++ Seq(ex).gss
    } else {
      Seq()
    }
    val pickups = if (health <= 0){
      Seq(ItemPickup(pos, Trampoline).toPickup, ToolPickup.create(pos |+ 1, Gouge.toTool))
    } else {
      Seq()
    }
    val mixAmt = (t % Obelisk.attackTime)/Obelisk.attackTime.toDouble
    (copy(mixAmt = mixAmt), pos, spawns ++ pickups.gss)
  }

  def getTg = {
    tg.smap{t => t.mapFg(_.mix(Color.Red.dim(3), mixAmt))}
  }
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <|| (getTg |++| ij)
  }

}
