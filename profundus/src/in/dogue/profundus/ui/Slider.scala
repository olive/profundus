package in.dogue.profundus.ui

import in.dogue.antiqua.graphics.{TileFactory, TextFactory, TileRenderer, Tile}
import in.dogue.profundus.input.Controls
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.mode.loadout.Loadout

object Slider {
  def create(i:Int, j:Int,
             icon:TileGroup, values:Int=>TileGroup,
             fillIn:Int => Loadout => Loadout,
             max:Int, value:Int, cost:Int, incr:Int) = {
    val up = CP437.▲.mkTile(Color.Black, Color.White)
    val down = CP437.▼.mkTile(Color.Black, Color.White)
    Slider(i, j, icon, values, fillIn, up, down, max, value, cost, incr)
  }
}

case class Slider private (i:Int, j:Int,
                           icon:TileGroup, values:Int=>TileGroup,
                           private val fillIn:Int => Loadout => Loadout,
                           up:Tile, down:Tile,
                           max:Int, value:Int, cost:Int, incr:Int) {
  def update(rem:Int):(Slider, Int) = {
    Controls.AxisY.zip(15,3) match {
      case -1 if rem >= cost && value < max => (copy(value=value+incr), rem-cost)
      case 1 if value >= 1 => (copy(value=value.drop(incr)), rem+cost)
      case _ => (this, rem)
    }
  }


  def doFill(lo:Loadout):Loadout = fillIn(value)(lo)

  private def drawSelection(selected:Boolean, rect:Recti)(tr:TileRenderer):TileRenderer = {
    if (!selected) {
      tr
    } else {
      val tf = TileFactory(Color.Black, Color.White)
      val i0 = i - (icon.length > 0).select(0, 1)
      (tr <| (i0, j, tf(CP437.`[`))
          <| (i + rect.width + 1, j, tf(CP437.`]`))
        )
    }
  }

  private def drawArrows(selected:Boolean, rem:Int, x:Int)(tr:TileRenderer):TileRenderer = {
    def f(t:Tile) = if (!selected) t.setFg(Color.DarkGrey) else t
    val upDraw = (i+x, j - 1, f(up)).onlyIf(rem >= cost)
    val downDraw =  (i+x, j + 1, f(down)).onlyIf(value >= 1)
    tr <+? upDraw <+? downDraw
  }

  def draw(selected:Boolean, rem:Int)(tr:TileRenderer):TileRenderer = {
    val spr = values(value)
    val span = spr.getSpan
    val center = span.center
    (tr <++ (icon |+| (i, j))
        <+< drawArrows(selected, rem, center.x+1)
        <++ (spr |+| (i+1, j))
        <+< drawSelection(selected, span)
      )
  }
}
