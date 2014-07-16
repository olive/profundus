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
    val r = new Random()
    def f(r:Random):Tile = {
      val bg = Red.dim(6 + r.nextDouble)
      val fg = Red.dim(3 + r.nextDouble)
      val code = Vector(CP437.`-`, CP437.`=`, CP437.`≡`, CP437.` `).randomR(r)
      code.mkTile(bg, fg)
    }
    val rect = Rect.createTextured(cols, rows, f, r)
    val b = Profundus.border(cols, rows)
    def mk(s:String) = Profundus.tf.create(s).toTileGroup
    val draws = Seq(
      mk("Name: " + pl.name) |+| (1,1),
      mk("Loadout: Feisty") |+| (1,3),
      mk("Killed by: Demons") |+| (1, 5),
      mk("Ropes used        : " + pl.ropesUsed) |+| (1, 7),
      mk("Capsules used     : " + pl.bombsUsed) |+| (1, 8),
      mk("Fuel used         : " + pl.fuelUsed) |+| (1, 9),
      mk("Minerals traded   : " + pl.gemsSpent) |+| (1, 10),
      mk("Minerals got      : " + pl.gemsCollected) |+| (1, 11),
      mk("Pounds earth moved: " + pl.tilesDug) |+| (1, 13),
      mk("Lived for         : " + pl.timeSpent) |+| (1, 14),
      mk("Depth reached     : " + pl.deepest) |+| (1, 15),
      mk("You will be remembered") |+| (4,41),
      mk("      as a Fool.") |+| (4,42)
    )

    ResultMode(cols, rows, pl, rect, b, draws.flatten)
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
