package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.profundus.world._
import in.dogue.profundus.lighting.LightSource
import scala.util.Random
import in.dogue.profundus.entities.damagezones.SingleTileZone
import in.dogue.antiqua.data.{CP437, Direction}
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.damagezones.SingleTileZone
import in.dogue.profundus.world.WorldTile
import in.dogue.profundus.audio.SoundManager

object Stalactite {
  def create(ij:Cell, sp:WorldTile) = {
    val light = LightSource.createCircle(ij, 0, 0, 0)
    val st = Stalactite(sp.tile, false, 0)
    StandardEntity.create[Stalactite](_.update, _.draw, _.onMove, st, light, false, None, 1000, new Random()/*FIXME: should be okay, never used*/).toEntity(ij)
  }
}

case class Stalactite(tile:Tile, destroy:Boolean, killT:Int) {
  import Profundus._

  def onMove(adj:Direction => Option[WorldTile]) = {
    if (adj(Direction.Down).isDefined) {

      copy(destroy=true)
    } else {
      this
    }
  }

  def update(id:EntityId, health:Int, t:Int, pos:Cell, cache:TerrainCache, pi:PlayerInfo, r:Random):(Stalactite, Cell, Seq[GlobalMessage]) = {
    val kz = SingleTileZone(pos --> Direction.Down, 100, DamageType.Spikes).toZone
    if (killT == 1) {
      SoundManager.enehit.play(pos)
    }
    val sk = if (killT > 6) {
      println("destroy! " + killT)
      id.gss
    } else {
      Seq()
    }
    copy(killT=killT+destroy.select(0,1)) @@ pos @@ (Seq(kz).gss ++ sk)
  }

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <| (ij, tile)
  }
}
