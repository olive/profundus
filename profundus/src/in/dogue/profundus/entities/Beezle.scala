package in.dogue.profundus.entities

import in.dogue.profundus.lighting.LightSource
import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.{WorldSpawn, TerrainCache}
import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.deformations.ExplosionDeformation
import in.dogue.profundus.particles.ExplosionParticle
import in.dogue.profundus.entities.damagezones.ExplosionZone
import in.dogue.profundus.Profundus

object Beezle {
  def create(ij:Cell, r:Random) = {
    val light = LightSource.createCircle(ij, 0, 5, 0.5)
    val tile = CP437.*.mkTile(Color.Black, Color.Red)
    val hp = 30
    val self = Beezle(tile, hp, false)
    StandardEntity.create[Beezle](_.update, _.draw, StandardEntity.NoMove, self, light, true, None, hp, r).toEntity(ij)
  }
}

case class Beezle(tile:Tile, maxHealth:Int, exploded:Boolean) {
  def update(health:Int, t:Int, pos:Cell, cache:TerrainCache, pi:PlayerInfo, r:Random):(Beezle, Cell, Seq[WorldSpawn]) = {
    val ppos = pi.pos
    val pState = pi.live
    import Profundus._
    val dd = ppos |-| pos
    val close = dd.mag < 10
    val adjacent = dd.x.abs + dd.y.abs == 1
    val inside = dd.x.abs + dd.y.abs == 0
    val newPos = if (inside) {
      pos -| 1
    } else if (adjacent) {
      pos
    } else if (cache.hasLineOfSight(ppos, pos) && close && t % 12 == 0) {
      val move = dd.signum
      pos |+| move
    } else {
      pos
    }

    val (gs, hasExploded) = if (!exploded && health < maxHealth && pState == Alive) {
      val `def` = ExplosionDeformation.create(pos, Damage(50, DamageType.Explosion), 8, 3).toDeformation

      val par = ExplosionParticle.create(pos, 8, 3).toParticle

      val zone = ExplosionZone.create(pos, 8, 3, DamageType.Beezle).toZone
      val gs = Seq(`def`).gss ++ Seq(par).gss ++ Seq(zone).gss
      (gs, true)
    } else {
      (Seq(), false)
    }

    copy(exploded=hasExploded) @@ newPos @@ gs
  }

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+ (ij, tile)
  }
}
