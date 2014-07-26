package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.profundus.world.{GlobalMessage, TerrainCache, WorldTile, Spike}
import in.dogue.profundus.lighting.LightSource
import scala.util.Random
import in.dogue.profundus.entities.damagezones.SingleTileZone
import in.dogue.antiqua.data.{CP437, Direction}
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color

object Stalactite {
  def create(ij:Cell, sp:WorldTile) = {
    val light = LightSource.createCircle(ij, 0, 0, 0)
    StandardEntity.create[Stalactite](_.update, _.draw, _.onMove, Stalactite(sp.tile), light, false, None, 1000, new Random()/*FIXME: should be okay, never used*/).toEntity(ij)
  }
}

case class Stalactite(t:Tile) {
  import Profundus._

  def onMove(adj:Direction => Option[WorldTile]) = {
    if (adj(Direction.Down).isDefined) {
      copy(t=CP437.a.mkTile(Color.Black, Color.Black))
    } else {
      this
    }
  }

  def update(health:Int, t:Int, pos:Cell, cache:TerrainCache, pi:PlayerInfo, r:Random):(Stalactite, Cell, Seq[GlobalMessage]) = {
    val kz = SingleTileZone(pos --> Direction.Down, 100, DamageType.Spikes).toZone
    this @@ pos @@ Seq(kz).gss
  }

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <| (ij, t)
  }
}
