package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.{Filter, Tile, Text, TileRenderer}
import in.dogue.profundus.input.Controls
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua.Cell

object PauseMode {
  def create(m:Mode[_]) = {
    val text = Profundus.tf.create("PAUSE")
    PauseMode(m, text, 0)
  }
}
case class PauseMode(m:Mode[_], text:Text, t:Int) {
  def update = {
    val newPause = copy(t=t+1).toMode
    if (Controls.Pause.justPressed) {
      println("unpause")
      m
    } else {
      newPause
    }
  }
  def draw(tr:TileRenderer):TileRenderer = {
    def dim(c:Cell)(t:Tile) = t.mapBg(_.dim(5)).mapFg(_.dim(5))
    tr.withFilter(Filter(dim)) { t => t <+< m.draw } <+< text.draw(16 - text.length/2, 24)
  }

  def toMode:Mode[PauseMode] = Mode(_.update, _.draw, this)
}
