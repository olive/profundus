package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.{Tile, Rect, Border, TileRenderer}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.input.Controls
import scala.util.Random
import in.dogue.profundus.Profundus

object TitleMode {
  def create(cols:Int, rows:Int) = {
    def mk(r:Random) = {
      val code = Vector(CP437.`.`, CP437.`,`, CP437.`'`, CP437.`"`).randomR(r)
      val bg = Color.Brown.dim(4 + r.nextDouble)
      val fg = Color.Tan.dim(1 + r.nextDouble)
      code.mkTile(bg, fg)
    }
    val border = Profundus.border(cols, rows)
    val rect = Rect.createTextured(cols, rows, mk, new Random())
    val title = Tile.groupFromFile("profundusmap2", "tiles", CP437.intToCode, _.mkTile(Color.Brown.dim(4), Color.Tan.dim(1))).filter { case (i, j, t) =>
    t.code != CP437.` `.toCode}
    TitleMode(cols, rows, border, rect, title)
  }
}

case class TitleMode private (cols:Int, rows:Int, border:Border, r:Rect, title:TileGroup) {

  def update:Mode[_] = {
    if (Controls.Space.justPressed) {
      CircleTransition.create(cols, rows, this.toMode, LoadoutMode.create(cols, rows, None).toMode).toMode
    } else {
      this.toMode
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< r.draw(0,0) <+< border.draw(0, 0) <++ (title |+| (9,7))
  }

  def toMode = Mode[TitleMode](_.update, _.draw, this)
}
