package in.dogue.profundus.mode.loadout

import in.dogue.antiqua.graphics._
import in.dogue.profundus.input.Controls
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.ui.{Slider, Hud}
import in.dogue.profundus.entities._
import scala.util.Random
import in.dogue.antiqua.graphics.Text
import in.dogue.profundus.world.{Difficulty, Easy, Normal, Hard}
import in.dogue.profundus.Profundus
import in.dogue.profundus.mode.{StoryMode, Mode, GameMode, CircleTransition}

object LoadoutMode {
  val topp = 16
  def create(cols:Int, rows:Int, initial:Option[Loadout]) = {
    val pts = 210
    def mk(r:Random) = {
      import Color._
      val bg = Grey.dim(6 + r.nextDouble)
      val fg = Grey.dim(3 + r.nextDouble)
      val code = Vector(CP437.`-`, CP437.`=`, CP437.`≡`, CP437.` `).randomR(r)
      code.mkTile(bg, fg)
    }
    val rect = Rect.createTextured(cols, rows, mk, new Random(0))
    val tf = TextFactory(Color.Black, Color.White, CP437.unicodeToCode)
    val lo = initial.getOrElse(Loadout.default)
    val (rem, std) = Loadout.makeSliders(pts, lo)
    val (rem2, tool) = Loadout.makeTool(rem, lo)
    val (rem3, diff) = Loadout.makeDiff(rem2, lo)
    val ptText = tf.create("Remaining: ")
    val sliders =  std ++ Vector(tool, diff)
    val lines = Vector(
      "Carefully prepare for your",
      "journey. Once you descend",
      "into the abyss there is no",
      "coming back.",
      "Should your body ever be",
      "recovered, you are likely",
      "to be pitied as a fool and",
      "forgotten.",
      "",
      "But if the stories",
      "are true...▀"
    )
    import LoadoutSection.{create => cr}
    val desc =  cr(cols, topp, 0, 0, lines, tf, (2,2))
    val offset = (1,1)
    val top =   cr(   cols,              5,       0,    0+topp, Vector(""), tf, offset)
    val left =  cr(cols-14,             19,       0,    5+topp, Vector("Provisions"), tf, offset)
    val right = cr(     14,             19, cols-14,    5+topp, Vector("Tools"), tf, offset)
    val bott  = cr(   cols, rows-19-5-topp,       0, 5+19+topp, Vector("Cavern Severity"), tf, offset)
    val secs = Vector(desc, top, left, right, bott)
    LoadoutMode(cols, rows, tf, sliders, 0, ptText, rem3, rect, secs)
  }
}



case class LoadoutMode private (cols:Int, rows:Int, tf:TextFactory, sliders:IndexedSeq[Slider], ptr:Int, ptText:Text, points:Int, r:Rect, los:Seq[LoadoutSection]) {

  private def move:LoadoutMode = {
    Controls.AxisX.zip(15,5) match {
      case 1 if ptr < sliders.length - 1 => copy(ptr=ptr+1)
      case -1 if ptr > 0 => copy(ptr = ptr.drop1)
      case _ => this
    }
  }
  def update = {
    if (Controls.Story.justPressed) {
      CircleTransition.create(cols, rows, this.toMode, StoryMode.create(cols, rows, getLoadout).toMode).toMode
    } else if (Controls.Space.justPressed) {
      CircleTransition.create(cols, rows, this.toMode, GameMode.create(cols, rows, getLoadout).toMode).toMode
    } else {
      val (newS, newP) = sliders(ptr).update(points)
      val newSliders = sliders.updated(ptr, newS)
      copy(sliders=newSliders, points=newP).move.toMode
    }
  }

  private def getLoadout = {
    sliders.foldLeft(Loadout.default) { case (lo, sl) =>
      sl.doFill(lo)
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    val ptX = 8
    val ptY = 2 + LoadoutMode.topp
    (tr <+< r.draw(0,0)
        <++< sliders.zipWithIndex.map { case (s, i) => s.draw(i == ptr, points) _}
        <+< ptText.draw(ptX, ptY)
        <+< tf.create("%3s☼".format(points.toString)).draw(ptX + ptText.length, ptY)
        <++< los.map {_.draw _}
      )
  }
  def toMode:Mode[LoadoutMode] = Mode(_.update, _.draw, this)
}
