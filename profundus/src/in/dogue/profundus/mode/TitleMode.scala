package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.{Rect, Border, TileRenderer}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.input.Controls

object TitleMode {
  def create(cols:Int, rows:Int) = {
    val border = Border.standard(CP437.doubleBorder, Color.Black, Color.White)(cols, rows)
    val rect = Rect.createPlain(cols, rows, CP437.u.mkTile(Color.Black, Color.Yellow))
    TitleMode(cols, rows, border, rect)
  }
}

case class TitleMode private (cols:Int, rows:Int, border:Border, r:Rect) {

  def update:Mode[_] = {
    if (Controls.Space.justPressed) {
      Transition.create(cols, rows, this.toMode, GameMode.create(cols, rows).toMode).toMode
    } else {
      this.toMode
    }
  }

  def draw(tr:TileRenderer):TileRenderer = tr <+< r.draw(0,0) <+< border.draw(0, 0)

  def toMode = Mode[TitleMode](_.update, _.draw, this)
}
