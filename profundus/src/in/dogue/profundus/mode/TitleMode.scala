package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.{Rect, Border, TileRenderer}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.input.Controls
import scala.util.Random

object TitleMode {
  def create(cols:Int, rows:Int) = {
    def mk(r:Random) = {
      val code = Vector(CP437.`.`, CP437.`,`, CP437.`'`, CP437.`"`).randomR(r)
      val bg = Color.Brown.dim(4 + r.nextDouble)
      val fg = Color.Tan.dim(1 + r.nextDouble)
      code.mkTile(bg, fg)
    }
    val border = Border.standard(CP437.doubleBorder, Color.Black, Color.White)(cols, rows)
    val rect = Rect.createTextured(cols, rows, mk, new Random())
    TitleMode(cols, rows, border, rect)
  }
}

case class TitleMode private (cols:Int, rows:Int, border:Border, r:Rect) {

  def update:Mode[_] = {
    if (Controls.Space.justPressed) {
      CircleTransition.create(cols, rows, this.toMode, LoadoutMode.create(cols, rows, None).toMode).toMode
    } else {
      this.toMode
    }
  }

  def draw(tr:TileRenderer):TileRenderer = tr <+< r.draw(0,0) <+< border.draw(0, 0)

  def toMode = Mode[TitleMode](_.update, _.draw, this)
}
