package in.dogue.profundus.particles

import in.dogue.profundus.world.TerrainCache
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.graphics.{TileFactory, Tile, TileRenderer}
import in.dogue.profundus.entities.Falling
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
object WaterDrop {
  val a = 9.8/60
  def create(ij:Cell) = {
    val tf = TileFactory(Color.Black, Color.DarkGrey)
    val up   = tf(CP437.`'`)
    val mid  = tf(CP437.`·`)
    val down = tf(CP437.`·`)
    WaterDrop(ij, ij.y, ij.y, up, mid, down, false, 0)
  }
}

case class WaterDrop private (ij:Cell, j0:Double, j:Double, up:Tile, mid:Tile, down:Tile, dead:Boolean, t:Int) {
  import WaterDrop._
  def update(tc:TerrainCache) = {
    val newJ = j0 + 0.5*a*t*t
    val newIj = (ij.x, (j + (newJ - j).clamp(0,1)).toInt)
    if (tc.isSolid(newIj)) {
      copy(dead=true)
    } else {
      copy(t=t+1, j=newJ, ij = newIj)
    }

  }

  private def getTile:Tile = {
    val pc = j % 1.0
    if (pc < 0.33) {
      up
    } else if (pc < 0.66) {
      mid
    } else {
      down
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <| (ij, getTile)
  }

  def isDone = dead
  def getLight = Seq()
  def toParticle:Particle = Particle[WaterDrop](_.update, _.draw, _.getLight, _.isDone, this)
}
