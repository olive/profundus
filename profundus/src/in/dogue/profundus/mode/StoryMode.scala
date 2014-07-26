package in.dogue.profundus.mode

import in.dogue.profundus.ui.{MessageBoxComplete, MessageBoxContinue, MessageBox}
import in.dogue.profundus.Profundus
import in.dogue.profundus.mode.loadout.{Loadout, LoadoutMode}
import in.dogue.antiqua.graphics.{Tile, Rect, TileRenderer}
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import java.nio.file.{Paths, Files}
import java.nio.charset.Charset
import com.badlogic.gdx.Gdx
import com.deweyvm.gleany.Glean
import in.dogue.profundus.utils.MessageBoxReader

object StoryMode {
  def create(cols:Int, rows:Int, lo:Loadout) = {
    val lines = MessageBoxReader.load("story")
    def getMode():() => Mode[_] = () => LoadoutMode.create(cols, rows, lo.some).toMode
    val mb = MessageBox.create(Profundus.tf, lines, getMode, Seq())
    val rect = Rect.createPlain(cols, rows, CP437.`#`.mkTile(Color.Brown.dim(10), Color.Black))
    val arrow = CP437.►.mkTile(Color.Black, Color.White)
    StoryMode(cols, rows, rect, lo, arrow, mb, 0)
  }
}
case class StoryMode private (cols:Int, rows:Int, b:Rect, lo:Loadout, arrow:Tile, mb:MessageBox[() => Mode[_]], t:Int) {
  def update:Mode[_] = mb.update match {
    case MessageBoxContinue(mb) => copy(mb=mb, t=t+1).toMode
    case MessageBoxComplete(mode) =>
      CircleTransition.create(cols, rows, this.toMode, mode, "StoryMode=>LoadoutMode").toMode
  }

  def drawArrow(ij:Cell) = {

    (ij, arrow).onlyIf(t % 60 < 30 && mb.pagePending)

  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< b.draw((0,0)) <+< mb.draw((2,10)) <|? drawArrow((cols - 2, 10 + mb.height))
  }

  def toMode:Mode[StoryMode] = Mode(_.update, _.draw, this)
}
