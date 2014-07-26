package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.{Transaction, WorldSpawn, TerrainCache}
import scala.util.Random
import in.dogue.profundus.lighting.LightSource
import in.dogue.antiqua.graphics._
import in.dogue.profundus.Profundus
import Profundus._
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.{Direction, CP437}
import in.dogue.profundus.input.Controls
import in.dogue.profundus.ui.MessageBox
import in.dogue.antiqua.graphics.Border
import in.dogue.profundus.utils.MessageBoxReader
import in.dogue.profundus.entities.pickups.{Pickup, ToolPickup}

object Shopkeeper {
  def create(ij:Cell, item:Pickup, r:Random) = {
    val tile = CP437.S.mkTile(Color.Black, Color.Yellow)
    val border = Border(CP437.doubleBorder)(Color.Black, Color.White)(20, 10).toTileGroup
    val bg = Rect.createPlain(20, 10, CP437.` `.mkTile(Color.Black, Color.Black)).toTileGroup
    val enough = MessageBoxReader.load("shopkeeper_enough")
    val lacking = MessageBoxReader.load("shopkeeper_lacking")
    val out = MessageBoxReader.load("shopkeeper_out")
    def mkBox(vb:Vector[String]) = MessageBox.create(Profundus.tf, vb, () => (), (bg ++ border) |++| ((-2, -2)))
    val eBox = mkBox(enough)
    val lBox = mkBox(lacking)
    val oBox = mkBox(out)
    val arrow = CP437.↑.mkTile(Color.Black, Color.White)
    val light = LightSource.createCircle(ij, 0, 0, 0)
    val cost = 1
    val trans = Transaction(-cost, item) _
    val sk = new Shopkeeper(tile, arrow, eBox, lBox, oBox, cost, trans, false, false, false, 0)
    StandardEntity.create[Shopkeeper](_.update, _.draw, sk, light, false, None, 100, r).toEntity(ij)
  }
}
case class Shopkeeper(tile:Tile, arrow:Tile,
                      enough:MessageBox[Unit], lacking:MessageBox[Unit], out:MessageBox[Unit],
                      cost:Int, trans:Cell => Transaction,
                      bought:Boolean, aggroed:Boolean, close:Boolean,
                      tt:Int) {
  def update(health:Int, t:Int, pos:Cell, cache:TerrainCache, pi:PlayerInfo, r:Random):(Shopkeeper, Cell, Seq[WorldSpawn]) = {
    val ppos = pi.pos
    val isClose = (pos |-| ppos).mag < 5 && !aggroed
    val showMb = Controls.Up.justPressed && isClose
    val (buy, spawns) = if (showMb) {
      (if (bought) {
        true @@ out
      } else if (pi.numMinerals >= cost && !bought) {
        true @@ enough
      } else {
        false @@ lacking
      }).doo{ case (doBuy, box) =>
        val itemPos = if (!cache.isSolid(ppos --> Direction.Left)) {
          ppos --> Direction.Left
        } else if (!cache.isSolid(ppos --> Direction.Right)) {
          ppos --> Direction.Right
        } else {
          ppos
        }
        doBuy @@ (GameBox(pos |+| ((-6, 3)), box).gss ++ trans(itemPos).gss)
      }

    } else {
      false @@ Seq()
    }
    copy(tt=tt+1, close=isClose, bought=buy) @@ pos @@ spawns
  }

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+ (ij, tile) <+? (ij -| 2, arrow).onlyIf(close && tt % 30 < 15)
  }
}
