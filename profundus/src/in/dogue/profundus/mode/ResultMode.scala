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
import in.dogue.profundus.mode.loadout.LoadoutMode

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
    def mk(s:String) = Profundus.tf.create(s).filterToTileGroup(CP437.notBlank)
    val x0 = 2
    val y0 = 2
    val draws = Seq(
      mk("Name: " + pl.lo.name) |++| ((x0,y0)),
      mk("Loadout: Feisty") |++| ((x0,y0+2)),
      mk("Killed by " + pl.getKilledBy) |++| ((x0, y0+4)),
      mk("Ropes used        : " + pl.ropesUsed) |++| ((x0, y0+6)) ,
      mk("Capsules used     : " + pl.bombsUsed) |++| ((x0, y0+7)),
      mk("Fuel used         : " + pl.fuelUsed) |++| ((x0, y0+8)),
      mk("Minerals traded   : " + pl.gemsSpent) |++| ((x0, y0+9)),
      mk("Minerals got      : " + pl.gemsCollected) |++| ((x0, y0+10)),
      mk("Pounds earth moved: " + pl.tilesDug) |++| ((x0, y0+12)),
      mk("Lived for         : " + pl.timeString) |++| ((x0, y0+13)),
      mk("Depth reached     : " + pl.deepest) |++| ((x0, y0+14)),
      mk("A pathetic end.") |++| ((8,40)),
      mk("Only a fool-") |++| ((10,42)),
      mk("would believe such stories.") |++| ((2,43))
    )

    ResultMode(cols, rows, pl, rect, b, draws.flatten)
  }
}

case class ResultMode private (cols:Int, rows:Int, pl:PlayerLog, r:Rect, b:Border, draws:TileGroup) {

  def update = {
    if (Controls.Space.justPressed) {
      val f = () => LoadoutMode.create(cols, rows, pl.lo.some).toMode
      CircleTransition.create(cols, rows, this.toMode, f, "LoadoutMode=>ResultMode").toMode
    } else {
      this.toMode
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< r.draw((0,0)) <+< b.draw((0,0)) <|| draws
  }

  def toMode:Mode[ResultMode] = Mode(_.update, _.draw, this)
}
