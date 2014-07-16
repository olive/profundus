package in.dogue.profundus.mode

import in.dogue.profundus.entities.PlayerLog
import in.dogue.antiqua.graphics._
import in.dogue.profundus.input.Controls
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.Profundus
import scala.util.Random
import com.deweyvm.gleany.graphics.Color._
import in.dogue.antiqua.data.CP437

object ResultMode {
  def create(cols:Int, rows:Int, pl:PlayerLog) = {
    import Profundus.tf
    val r = new Random()
    def f(r:Random):Tile = {
      val bg = Red.dim(6 + r.nextDouble)
      val fg = Red.dim(3 + r.nextDouble)
      val code = Vector(CP437.`-`, CP437.`=`, CP437.`≡`, CP437.` `).randomR(r)
      code.mkTile(bg, fg)
    }
    val rect = Rect.createTextured(cols, rows, f, r)
    val b = Profundus.border(cols, rows)
    val name = tf.create(pl.name).toTileGroup |+| (1,1)
    ResultMode(cols, rows, pl, rect, b, Seq(name).flatten)
  }
}

case class ResultMode private (cols:Int, rows:Int, pl:PlayerLog, r:Rect, b:Border, draws:TileGroup) {

  def update = {
    if (Controls.Space.justPressed) {
      CircleTransition.create(cols, rows, this.toMode, LoadoutMode.create(cols, rows, pl.lo.some).toMode).toMode
    } else {
      this.toMode
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< r.draw(0,0) <+< b.draw(0,0) <|| draws
  }

  def toMode:Mode[ResultMode] = Mode(_.update, _.draw, this)
}
