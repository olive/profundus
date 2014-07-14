package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.input.Controls
import in.dogue.antiqua.Antiqua
import Antiqua._

object DeadMode {
  def create(cols:Int, rows:Int, mode:GameMode, lo:Loadout) = DeadMode(cols, rows, mode, lo)
}

case class DeadMode private (cols:Int, rows:Int, mode:GameMode, lo:Loadout) {

  def update = {
    if (Controls.Space.justPressed) {
      CircleTransition.create(cols, rows, this.toMode, LoadoutMode.create(cols, rows, lo.some).toMode).toMode
    } else {
      copy(mode = mode.selfUpdate).toMode
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< mode.draw
  }

  def toMode:Mode[DeadMode] = Mode(_.update, _.draw, this)
}
