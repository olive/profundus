package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.input.Controls

object DeadMode {
  def create(cols:Int, rows:Int, mode:GameMode) = DeadMode(cols, rows, mode)
}

case class DeadMode private (cols:Int, rows:Int, mode:GameMode) {

  def update = {
    if (Controls.Space.justPressed) {
      CircleTransition.create(cols, rows, this.toMode, TitleMode.create(cols, rows).toMode).toMode
    } else {
      copy(mode = mode.selfUpdate).toMode
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< mode.draw
  }

  def toMode:Mode[DeadMode] = Mode(_.update, _.draw, this)
}
