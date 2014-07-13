package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.{Border, TileRenderer}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color


object TitleMode {
  def create(cols:Int, rows:Int) = {
    val border = Border.standard(CP437.doubleBorder, Color.Black, Color.White)(cols, rows)
    TitleMode(cols, rows, border)
  }
}

case class TitleMode private (cols:Int, rows:Int, border:Border) {

  def update:Mode[_] = toMode

  def draw(tr:TileRenderer):TileRenderer = tr <+< border.draw(0, 0)

  def toMode = Mode[TitleMode](_.update, _.draw, this)
}
