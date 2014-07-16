package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.input.Controls
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.PlayerLog

object DeadMode {
  def create(cols:Int, rows:Int, mode:GameMode, log:PlayerLog) = DeadMode(cols, rows, mode, log)
}

case class DeadMode private (cols:Int, rows:Int, mode:GameMode, log:PlayerLog) {

  def update = {
    if (Controls.Space.justPressed) {
      CircleTransition.create(cols, rows, this.toMode, ResultMode.create(cols, rows, log).toMode).toMode
    } else {
      copy(mode = mode.selfUpdate).toMode
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< mode.draw
  }

  def toMode:Mode[DeadMode] = Mode(_.update, _.draw, this)
}
