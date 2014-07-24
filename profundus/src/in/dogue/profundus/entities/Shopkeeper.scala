package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.{GlobalSpawn, TerrainCache}
import scala.util.Random
import in.dogue.profundus.lighting.LightSource
import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.profundus.Profundus
import Profundus._
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437

object Shopkeeper {
  def create(ij:Cell, r:Random) = {
    val tile = CP437.S.mkTile(Color.Black, Color.Yellow)
    val sk = new Shopkeeper(tile)
    val light = LightSource.createCircle(ij, 0, 0, 0)
    StandardEntity.create[Shopkeeper](_.update, _.draw, sk, light, false, None, 100, r).toEntity(ij)
  }
}
class Shopkeeper(t:Tile) {
  def update(health:Int, t:Int, pos:Cell, cache:TerrainCache, ppos:Cell, pState:LivingState, r:Random):(Shopkeeper, Cell, Seq[GlobalSpawn]) = {
    this @@ pos @@ Seq()
  }

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+ (ij, t)
  }
}
