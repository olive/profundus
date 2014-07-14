package in.dogue.profundus.mode

import in.dogue.profundus.entities.PlayerLog
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.input.Controls
import in.dogue.antiqua.Antiqua
import Antiqua._

object ResultMode {
  def create(cols:Int, rows:Int, pl:PlayerLog) = {
    ResultMode(cols, rows, pl)
  }
}

case class ResultMode private (cols:Int, rows:Int, pl:PlayerLog) {

  def update = {
    if (Controls.Space.justPressed) {
      CircleTransition.create(cols, rows, this.toMode, LoadoutMode.create(cols, rows, pl.lo.some).toMode).toMode
    } else {
      this.toMode
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr
  }

  def toMode:Mode[ResultMode] = Mode(_.update, _.draw, this)
}
