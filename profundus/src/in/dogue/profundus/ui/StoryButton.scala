package in.dogue.profundus.ui

import in.dogue.profundus.mode.loadout.{LoadoutMove, Loadout, LoadoutStay, LoadoutUpdate}
import in.dogue.profundus.input.Controls
import in.dogue.profundus.mode.{StoryMode, CircleTransition, Mode}
import in.dogue.antiqua.graphics.{TileFactory, Text, TileRenderer}
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.CP437

case class StoryButton(cols:Int, rows:Int, ij:Cell, text:Text) {
  def update(m:Mode[_], rem:Int):(LoadoutUpdate, StoryButton ,Int) = {
    if (Controls.Action.justPressed) {
      def mkTransition(lo:Loadout) = {
        val story = () => StoryMode.create(cols, rows, lo).toMode
        CircleTransition.create(cols, rows, m, story, "LoadoutMode=>StoryMode").toMode
      }

      (LoadoutMove(mkTransition), this, rem)
    } else {
      (LoadoutStay, this, rem)
    }
  }

  def fill(lo:Loadout) = lo
  def draw(selected:Boolean, rem:Int)(tr:TileRenderer) = {
    tr <+< text.drawFg(ij) <+< (if (selected) {
      val tf = TileFactory(Color.Black, Color.White)
      val left = tf(CP437.`[`)
      val right = tf(CP437.`]`)
      (t:TileRenderer) => {
        t <| (ij |- 1, left) <| (ij |+ text.length, right)
      }
    } else {
      id[TileRenderer]
    })
  }
  def toLoadoutButton:LoadoutButton[StoryButton] = LoadoutButton(_.update, _.fill, _.draw, this)
}
