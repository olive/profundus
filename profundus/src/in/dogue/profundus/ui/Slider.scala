package in.dogue.profundus.ui

import in.dogue.antiqua.graphics.{TileFactory, TextFactory, TileRenderer, Tile}
import in.dogue.profundus.input.Controls
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.mode.loadout.{LoadoutUpdate, LoadoutStay, LoadoutMode, Loadout}
import in.dogue.profundus.mode.Mode
import in.dogue.profundus.audio.SoundManager

object Slider {
  def create(ij:Cell,
             icon:TileGroup,
             desc:Int=>TileGroup,
             values:Int=>TileGroup,
             dr:TileGroup => TileRenderer => TileRenderer,
             fillIn:Int => Loadout => Loadout,
             getWidth:Int=>Int,
             max:Int, value:Int, cost:Int, incr:Int) = {
    val up = CP437.▲.mkTile(Color.Black, Color.White)
    val down = CP437.▼.mkTile(Color.Black, Color.White)
    Slider(ij, icon, desc, values, dr, fillIn, up, down, getWidth, max, value, cost, incr)
  }
}

case class Slider private (ij:Cell,
                           icon:TileGroup,
                           desc:Int=>TileGroup,
                           values:Int=>TileGroup,
                           dr:TileGroup => TileRenderer => TileRenderer,
                           private val fillIn:Int => Loadout => Loadout,
                           up:Tile, down:Tile,
                           getWidth:Int=>Int,
                           max:Int, value:Int, cost:Int, incr:Int) {
  final val i = ij.x
  final val j = ij.y
  val realCost = cost*incr
  def update(m:Mode[_], rem:Int):(LoadoutUpdate, Slider, Int) = {
    def play() = SoundManager.clack.playFull()
    val (slide, remaining) = Controls.AxisY.zip(15,3) match {
      case -1 if rem >= realCost && value < max =>
        play()
        (copy(value=value+incr), rem-realCost)
      case 1 if value >= 1 =>
        play()
        (copy(value=value.drop(incr)), rem+realCost)
      case _ => (this, rem)
    }
    (LoadoutStay, slide, remaining)
  }


  def doFill(lo:Loadout):Loadout = fillIn(value)(lo)

  private def drawSelection(selected:Boolean, rect:Recti)(tr:TileRenderer):TileRenderer = {
    if (!selected) {
      tr
    } else {
      val tf = TileFactory(Color.Black, Color.White)
      val i0 = i - (icon.length > 0).select(0, 1)
      (tr <| ((i0, j), tf(CP437.`[`))
          <| ((i + getWidth(value) + 1, j), tf(CP437.`]`))
        )
    }
  }

  private def drawArrows(selected:Boolean, rem:Int, x:Int)(tr:TileRenderer):TileRenderer = {
    def f(t:Tile) = if (!selected) t.setFg(Color.DarkGrey) else t
    val upDraw = ((i+x, j - 1), f(up)).onlyIf(rem >= realCost)
    val downDraw =  ((i+x, j + 1), f(down)).onlyIf(value >= 1)
    tr <|? upDraw <|? downDraw
  }

  def drawDescription(tr:TileRenderer):TileRenderer = {
    val d = desc(value)
    val spr = values(value)
    tr <+< dr((spr |++| ij |++ 1)) <|| (d |++| ij |++ 1)
  }


  def draw(selected:Boolean, rem:Int)(tr:TileRenderer):TileRenderer = {

    val span = Recti(0,0,getWidth(value),1)
    val center = span.center
    (tr <|| (icon |++| ij)
        <+< drawArrows(selected, rem, center.x+1)
        <+< drawDescription
        <+< drawSelection(selected, span)
      )
  }

  def toLoadoutButton:LoadoutButton[Slider] = LoadoutButton(_.update, _.doFill, _.draw, this)
}
