package in.dogue.profundus.mode

import in.dogue.antiqua.graphics._
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.input.Controls
import scala.util.Random
import in.dogue.profundus.{Game, Profundus}
import in.dogue.profundus.mode.loadout.LoadoutMode
import in.dogue.antiqua.graphics.Border

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
    val tf = TextFactory(Color.Black, Color.Tan, CP437.unicodeToCode)
    val disc = tf.multiline("@oleaffia\nhttp://dogue.in\nPre-alpha build")
    val version = tf.multiline(Game.version)
    TitleMode(cols, rows, border, rect, disc, version, title)
  }
}

case class TitleMode private (cols:Int, rows:Int, border:Border, r:Rect, disclaimer:TileGroup, version:TileGroup, title:TileGroup) {

  def update:Mode[_] = {
    if (Controls.Space.justPressed) {
      val f = () => LoadoutMode.create(cols, rows, None).toMode
      CircleTransition.create(cols, rows, this.toMode, f, None).toMode
    } else {
      this.toMode
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    (tr <+< r.draw(0,0)
        <+< border.draw(0, 0)
        <++ (title |+| (9,7+8))
        <|| (disclaimer |+| (2,rows - 3 - 2))
        <|| (version |+| (2, 2))
      )
  }

  def toMode = Mode[TitleMode](_.update, _.draw, this)
}
