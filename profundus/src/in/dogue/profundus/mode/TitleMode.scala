package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.Implicits
import Implicits._


object TitleMode {
  def create(cols:Int, rows:Int) = TitleMode(cols, rows)
}

case class TitleMode private (cols:Int, rows:Int) {

  def update:Mode[_] = toMode

  def draw(tr:TileRenderer):TileRenderer = tr

  def toMode = Mode[TitleMode](_.update, _.draw, this)
}
