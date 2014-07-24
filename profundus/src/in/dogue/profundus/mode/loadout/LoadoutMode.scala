package in.dogue.profundus.mode.loadout

import in.dogue.antiqua.graphics._
import in.dogue.profundus.input.Controls
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.ui.{StoryButton, LoadoutButton, Slider, Hud}
import in.dogue.profundus.entities._
import scala.util.Random
import in.dogue.antiqua.graphics.Text
import in.dogue.profundus.{Game, Profundus}
import in.dogue.profundus.mode.{StoryMode, Mode, GameMode, CircleTransition}
import in.dogue.profundus.procgen.PlayerInfo
import in.dogue.profundus.audio.SoundManager

object LoadoutMode {
  val topp = 15
  def create(cols:Int, rows:Int, initial:Option[Loadout]) = {
    val pts = 210
    def mk(r:Random) = {
      import Color._
      val bg = Grey.dim(9 + r.nextDouble)
      val fg = Grey.dim(6 + r.nextDouble)
      val code = Vector(CP437.`-`, CP437.`=`, CP437.`≡`, CP437.` `).randomR(r)
      code.mkTile(bg, fg)
    }
    val r = new Random()
    val rect = Rect.createTextured(cols, rows, mk, r)
    val tf = Profundus.tf
    val lo = initial.getOrElse(Loadout.default)
    val (rem, std) = Loadout.makeSliders(pts, lo)
    val (rem2, tool) = Loadout.makeTool(rem, lo)
    val ptText = tf.create("Remaining: ")
    val story = StoryButton(cols, rows, (26, 12), tf.create("More")).toLoadoutButton
    val sliders =  Vector(story) ++ std ++ Vector(tool)
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
      "are true..."
    )
    import LoadoutSection.{create => cr}
    val desc =  cr(cols, topp, (0, 0), lines, tf, (2,2))
    val offset = (1,1)
    val top =   cr(   cols,              5, (      0,    0+topp), Vector(""), tf, offset)
    val left =  cr(cols-14,             17, (      0,    5+topp), Vector("Provisions"), tf, offset)
    val right = cr(     14,             17, (cols-14,    5+topp), Vector("Tools"), tf, offset)
    val bott  = cr(   cols, rows-17-5-topp, (      0, 5+17+topp), Vector("Bio"), tf, offset)
    val secs = Vector(desc, top, left, right, bott)
    LoadoutMode(cols, rows, tf, sliders, 1, ptText, rem2, rect, new PlayerInfo(r), secs)
  }
}

sealed trait LoadoutUpdate
case object LoadoutStay extends LoadoutUpdate
case class LoadoutMove(nm:Loadout => Mode[_]) extends LoadoutUpdate

case class LoadoutMode private (cols:Int, rows:Int, tf:TextFactory, sliders:IndexedSeq[LoadoutButton[_]], ptr:Int, ptText:Text, points:Int, r:Rect, pi:PlayerInfo, los:Seq[LoadoutSection]) {

  private def move:LoadoutMode = {
    def play() = SoundManager.blap.play()
    Controls.AxisX.zip(15,5) match {
      case 1 if ptr < sliders.length - 1 =>
        play()
        copy(ptr=ptr+1)
      case -1 if ptr > 0 =>
        play()
        copy(ptr = ptr.drop1)
      case _ => this
    }
  }
  def update = {
    if (Controls.Story.justPressed) {
      val f = () => StoryMode.create(cols, rows, getLoadout).toMode
      CircleTransition.create(cols, rows, this.toMode, f, "LoadoutMode=>StoryMode").toMode
    } else if (Controls.Space.justPressed) {
      val seed = Game.getSeed
      val f = () => GameMode.create(cols, rows, getLoadout, seed).toMode
      CircleTransition.create(cols, rows, this.toMode, f, "GameMode, Seed: " + seed).toMode
    } else {
      val (newMode, newS, newP) = sliders(ptr).update(this.toMode, points)
      newMode match {
        case LoadoutStay =>
          val newSliders = sliders.updated(ptr, newS)
          copy(sliders=newSliders, points=newP).move.toMode
        case LoadoutMove(nm) => nm(getLoadout)
      }

    }
  }

  private def getLoadout = {
    val loadout = sliders.foldLeft(Loadout.default) { case (lo, sl) =>
      sl.doFill(lo)
    }
    loadout.copy(name=pi.title)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    val ptX = 8
    val ptY = 2 + LoadoutMode.topp
    val bioY = 40
    def create(s:String) = {
      tf.create(s).filterToTileGroup(t => t.code != CP437.` `.toCode)
    }
    (tr <+< r.draw((0,0))
        <++< sliders.zipWithIndex.map { case (s, i) => s.draw(i == ptr, points) _}
        <+< ptText.drawFg((ptX, ptY))
        <|| (create("%3s☼".format(points.toString))|++| ((ptX + ptText.length, ptY)))
        <|| (create(pi.title) |++| ((2, bioY)))
        <|| (create("Fears : " + pi.weakness) |++| ((2, bioY+2)))
        <|| (create("Job   : " + pi.job)      |++| ((2, bioY+3)))
        <|| (create("Craves: " + pi.craving)  |++| ((2, bioY+4)))
        <++< los.map {_.draw _}
      )
  }
  def toMode:Mode[LoadoutMode] = Mode(_.update, _.draw, this)
}
