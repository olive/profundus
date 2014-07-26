package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua.Cell
import scala.util.Random
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.profundus.world.{GlobalMessage, TerrainCache}
import in.dogue.profundus.lighting.LightSource
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.deformations.MineralDeformation
import in.dogue.profundus.Profundus

object Midas {
  def create(ij:Cell, r:Random) = {
    val self = Midas(CP437.`;`.mkTile(Color.Black, Color.Yellow))
    val light = LightSource.createCircle(ij, 0, 2, 0.2)
    StandardEntity.create[Midas](_.update, _.draw, StandardEntity.NoMove, self, light, false, None, 1, r).toEntity(ij)
  }
}

case class Midas(t:Tile) {
  def update(id:EntityId, health:Int, t:Int, pos:Cell, cache:TerrainCache, pi:PlayerInfo, r:Random):(Midas, Cell, Seq[GlobalMessage]) = {
    import Profundus._
    val newPos = if (t % 120 == 0) {
      val dir = Vector(Direction.Left, Direction.Right).randomR(r)
      if (!cache.isSolid(pos --> dir)) {
        pos --> dir
      } else {
        pos
      }
    } else {
      pos
    }

    val dz = if (cache.isRock(pos --> Direction.Down)) {
      Seq(MineralDeformation.create(pos --> Direction.Down).toDeformation).gss
    } else {
      Seq()
    }

    this @@ newPos @@ dz

  }

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <| (ij, t)
  }
}
