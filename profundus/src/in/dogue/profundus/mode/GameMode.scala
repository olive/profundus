package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer
import scala.util.Random
import in.dogue.profundus.entities._
import in.dogue.profundus.ui.Hud
import in.dogue.profundus.mode.loadout.Loadout
import in.dogue.profundus.world.GreatWorld

object GameMode {
  def create(cols:Int, rows:Int, lo:Loadout) = {
    val worldCols = cols*4
    val r = new Random(0)
    val hudHeight = 5
    val gw = GreatWorld.create(worldCols, rows - hudHeight, lo, r)
    val hud = Hud.create(cols, hudHeight, gw.p.inv)
    GameMode(cols, rows, gw, hud, r)
  }
}

case class GameMode private(cols:Int, rows:Int, gw:GreatWorld, hud:Hud, r:Random) {

  def update = {
    val updated = selfUpdate
    updated.gw.p.state match {
      case Dead => DeadMode.create(cols, rows, this, updated.gw.p.log).toMode
      case Alive => updated.toMode
    }
  }

  def selfUpdate:GameMode = {
    copy(gw=gw.update, hud=hud.withInventory(gw.p.inv).atDepth(gw.p.y))
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr.withMove(0, hud.height){ t =>
      t <+< gw.draw
    }.<+<(hud.draw)
  }

  def toMode:Mode[GameMode] = Mode(_.update, _.draw, this)
}
