package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer
import scala.util.Random
import in.dogue.profundus.entities._
import in.dogue.profundus.ui.Hud
import in.dogue.profundus.mode.loadout.Loadout
import in.dogue.profundus.world.GreatWorld
import in.dogue.profundus.input.Controls
import in.dogue.profundus.lighting.LightManager
import in.dogue.profundus.{Profundus, Game}
import in.dogue.antiqua.Antiqua
import Antiqua._

object GameMode {
  def create(cols:Int, rows:Int, lo:Loadout, seed:Int) = {
    val worldCols = cols*4
    val worldRows = rows*1
    val r = new Random(seed)
    val hudHeight = 6
    val gw = GreatWorld.create(worldCols, worldRows, cols, rows, lo, r)
    val hud = Hud.create(cols, hudHeight, gw.p.inv, gw.p.getStamBar, gw.p.getHealthBar)
    val help = HelpScreen.create
    GameMode(cols, rows, gw, hud, help, r)
  }
}

case class GameMode private (cols:Int, rows:Int, gw:GreatWorld, hud:Hud, help:HelpScreen, r:Random) {

  def update = {
    if (Controls.Pause.justPressed) {
      PauseMode.create(this.toMode).toMode
    } else {
      gw.p.state match {
        case Dead => DeadMode.create(cols, rows, this, gw.p.log).toMode
        case Alive => selfUpdate.toMode
      }
    }
  }

  def selfUpdate:GameMode = {
    val pl = gw.p
    val newHud = hud.withInventory(pl.inv)
                    .atDepth(pl.y)
                    .withStam(pl.getStamBar)
                    .withHealth(pl.getHealthBar)
                    .withBuff(pl.getBuffIcon)
                    .withItems(pl.getItems)
    copy(gw=gw.resetLm.update, hud=newHud)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr.withMove(0, hud.height){ t =>
      t <+< gw.draw
    }.<+<(hud.draw) <+?< (help.draw _).onlyIf(Controls.Help.isPressed)

  }

  def toMode:Mode[GameMode] = Mode(_.update, _.draw, this)
}
