package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer
import scala.util.Random
import in.dogue.profundus.entities._
import in.dogue.profundus.ui.Hud
import in.dogue.profundus.mode.loadout.Loadout
import in.dogue.profundus.world.GreatWorld
import in.dogue.profundus.input.Controls
import in.dogue.profundus.lighting.LightManager

object GameMode {
  def create(cols:Int, rows:Int, lo:Loadout, seed:Int) = {
    val worldCols = cols*4
    val r = new Random(seed)
    val hudHeight = 6
    val gw = GreatWorld.create(worldCols, rows - hudHeight, lo, r)
    val hud = Hud.create(cols, hudHeight, gw.p.inv, gw.p.getStamBar, gw.p.getHealthBar)
    GameMode(cols, rows, gw, hud, r)
  }
}

case class GameMode private (cols:Int, rows:Int, gw:GreatWorld, hud:Hud, r:Random) {

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
    copy(gw=gw.setLm(LightManager.create).update, hud=newHud)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr.withMove(0, hud.height){ t =>
      t <+< gw.draw
    }.<+<(hud.draw)
  }

  def toMode:Mode[GameMode] = Mode(_.update, _.draw, this)
}
