package in.dogue.profundus.entities

import in.dogue.antiqua.Antiqua._
import in.dogue.profundus.world.{Transaction, GlobalMessage, TerrainCache}
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
    val border = Profundus.border(20, 10).toTileGroup
    val bg = Rect.createPlain(20, 10, CP437.` `.mkTile(Color.Black, Color.Black)).toTileGroup
    val enough = MessageBoxReader.load("shopkeeper_enough")
    val lacking = MessageBoxReader.load("shopkeeper_lacking")
    val out = MessageBoxReader.load("shopkeeper_out")
    val attack = MessageBoxReader.load("shopkeeper_attack")
    def mkBox(vb:Vector[String]) = MessageBox.create(Profundus.tf, vb, () => (), (bg ++ border) |++| ((-2, -2)))
    val eBox = mkBox(enough)
    val lBox = mkBox(lacking)
    val oBox = mkBox(out)
    val aBox = mkBox(attack)
    val arrow = CP437.↑.mkTile(Color.Black, Color.White)
    val light = LightSource.createCircle(ij, 0, 0, 0)
    val cost = 20
    val trans = Transaction(-cost, item) _
    val sk = new Shopkeeper(tile, arrow, eBox, lBox, oBox, aBox, cost, trans, false, false, false, 0)
    StandardEntity.create[Shopkeeper](_.update, _.draw, StandardEntity.NoMove, sk, light, false, None, 1, r).toEntity(ij)
  }
}
case class Shopkeeper(tile:Tile, arrow:Tile,
                      enough:MessageBox[Unit], lacking:MessageBox[Unit], out:MessageBox[Unit], attacked:MessageBox[Unit],
                      cost:Int, trans:Cell => Transaction,
                      bought:Boolean, aggroed:Boolean, close:Boolean,
                      tt:Int) {
  private def mkBox(pos:Cell, box:MessageBox[Unit]) = {
    GameBox(pos |+| ((-6, 3)), box).gms
  }
  def update(health:Int, t:Int, args:EntityArgs):(Shopkeeper, Cell, Seq[GlobalMessage]) = {
    val ppos = args.ppos
    val pos = args.pos
    val isClose = args.distance2 < 5*5 && !aggroed
    val showMb = Controls.Up.justPressed && isClose
    if (health <= 0) {
      return this @@ pos @@ mkBox(pos, attacked)
    }
    val (buy, spawns) = if (showMb) {
      (if (bought) {
        true @@ out
      } else if (args.pi.numMinerals >= cost && !bought) {
        true @@ enough
      } else {
        false @@ lacking
      }).doo{ case (doBuy, box) =>
        val itemPos = ppos.map { p =>
          if (!args.tc.isSolid(p --> Direction.Left)) {
            p --> Direction.Left
          } else if (!args.tc.isSolid(p --> Direction.Right)) {
            p --> Direction.Right
          } else {
            p
          }
        }.getOrElse(throw new Exception("fixme"))
        doBuy @@ (mkBox(pos, box) ++ trans(itemPos).gms)
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
