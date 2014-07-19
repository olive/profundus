package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua.{TileGroup, Cell}
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import in.dogue.profundus.entities.damagezones.ExplosionZone
import in.dogue.profundus.world.{WorldSpawn, GlobalSpawn}
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.particles.{DeathParticle, Particle, ExplosionParticle}
import in.dogue.profundus.entities.pickups.ItemPickup

object Casque {
  val attackTime = 10*60
  def create(ij:Cell, r:Random) = {
    val tg = Tile.makeGroup(Vector(
      (0, 0, CP437.ç.toCode, Color.Black, Color.White),
      (0, 1, CP437.↑.toCode, Color.Black, Color.White)
    ))

    Casque(ij, tg, 50, math.abs(r.nextInt()), Alive)

  }
}
case class Casque private (ij:Cell, tg:TileGroup, health:Int, t:Int, live:LivingState) {
  import Profundus._

  def pos = ij +| 1
  def beHit(dmg:Int) = copy(health=health.drop(dmg))
  def update:(Casque, Seq[GlobalSpawn], Seq[WorldSpawn]) = {
    val spawns = if (t > 0 && t % Casque.attackTime == 0) {
      val ps = ExplosionParticle.create(ij.x, ij.y, 0, 8, 3).toParticle
      val ex = ExplosionZone.create(ij.x, ij.y, 8, 3).toZone
      Seq(Seq(ps).ns, Seq(ex).ns)
    } else {
      Seq()
    }
    val (newLive, pickups) = if (health <= 0){
      (Dead, Seq(ItemPickup(ij, Trampoline).toPickup))
    } else {
      (live, Seq())
    }
    (copy(t=t+1, live=newLive), spawns, Seq(pickups.ws))
  }

  def getDeathParticle:Particle[_] = DeathParticle.create(ij.x, ij.y, 60).toParticle

  def draw(tr:TileRenderer):TileRenderer = {
    tr <|| (tg |+| (ij.x, ij.y))
  }
}
