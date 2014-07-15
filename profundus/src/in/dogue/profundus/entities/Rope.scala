package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileFactory, Tile, TileRenderer}
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.world.{TerrainCache, World}
import in.dogue.antiqua.algebra.Monoid

sealed trait RopeState {
  val throwHeight = 6
  val maxLength = 8
  val dropSpeed = 5
  val flySpeed = 5
}

object FlyUp { def create(src:(Int,Int)) = FlyUp(src, 0, 0) }
case class FlyUp private (private val src:(Int,Int), len:Int, t:Int) extends RopeState {
  def incrLen = copy(len=len+1, t=0)
  def x = src.x
  def y = src.y
  def top = src -| len
}

object DropDown { def create(top:(Int,Int)) = DropDown(top, 0, 0) }
case class DropDown private (private val top:(Int,Int), len:Int, t:Int) extends RopeState {
  def incrLen = copy(len=len+1, t=0)
  def x = top.x
  def y = top.y
  def bot = top +| len
}

object Steady { def create(top:(Int,Int), len:Int) = Steady(top, len) }
case class Steady private (private val top:(Int,Int), len:Int) extends RopeState {
  def x = top.x
  def y = top.y
}

object Rope {
  def create(ij:(Int,Int), d:Direction) = {
    val tf = TileFactory(Color.Black, Color.White)
    val nub = tf(CP437.a)
    val top = tf(CP437.⌠)
    val mid = tf(CP437.│)
    val bot = tf(CP437.⌡)
    val state = if (d.isVertical) {
      FlyUp.create(ij)
    } else {
      DropDown.create(ij --> d)
    }
    Rope(state, nub, top, mid, bot)
  }
}

case class Rope private (state:RopeState, nubT:Tile, topT:Tile, midT:Tile, bottomT:Tile) {

  /** y2 > y1 */
  private def between(ij:(Int, Int), x:Int, y1:Int, y2:Int) = ij.x == x && ij.y >= y1 && ij.y <= y2

  def ropeContains(ij:(Int,Int)) = state match {
    case FlyUp(_, _, _) => false
    case DropDown(top, len, _) => between(ij, top.x, top.y, top.y + len)
    case Steady(top, len) => between(ij, top.x, top.y, top.y + len)
  }

  def update(tc:TerrainCache):Rope = {
    val newState:RopeState = state match {
      case f@FlyUp(_,_,_) => updateFlyUp(f, tc)
      case d@DropDown(_,_,_) => updateDropDown(d, tc)
      case s@Steady(_,_) => updateSteady(s)
    }

    copy(state=newState)
  }

  private def updateFlyUp(f:FlyUp, tc:TerrainCache) = {
    val newT = f.t + 1
    if (newT % f.flySpeed == 0) {
      val top = f.top
      if (tc.isSolid(top --> Direction.Up) || f.len + 1 == f.maxLength) {
        DropDown.create(top)
      } else {
        f.incrLen
      }
    } else {
      f.copy(t=newT)
    }
  }

  private def updateDropDown(d:DropDown, tc:TerrainCache) = {
    val newT = d.t + 1
    if (newT % d.dropSpeed == 0) {
      val bottom = d.bot
      if (tc.isSolid(bottom --> Direction.Down) || d.len + 1 == d.maxLength) {
        Steady.create(d.bot -| d.len, d.len)
      } else {
        d.incrLen
      }
    } else {
      d.copy(t=newT)
    }
  }

  private def updateSteady(s:Steady) = s


  private def drawFlyUp(f:FlyUp)(tr:TileRenderer):TileRenderer = {
    tr <+ (f.x, f.y - f.len, nubT)
  }

  private def drawDropDown(d:DropDown)(tr:TileRenderer):TileRenderer = {
    val top = (d.x, d.y, topT)
    val mid = for (i <- (d.y + 1) until (d.y + d.len)) yield {
      (d.x, i, midT)
    }
    val bot = (d.x, d.y + d.len, bottomT)
    tr <+~ top <++ mid <+~ bot
  }

  private def drawSteady(s:Steady)(tr:TileRenderer):TileRenderer = {
    val top = (s.x, s.y, topT)
    val mid = for (i <- (s.y + 1) until (s.y + s.len)) yield {
      (s.x, i, midT)
    }
    val bot = (s.x, s.y + s.len, bottomT)
    tr <+~ top <++ mid <+~ bot
  }


  def draw(tr:TileRenderer):TileRenderer = {
    val draw = state match {
      case f@FlyUp(src,l,_) => drawFlyUp(f) _
      case d@DropDown(top,_,_) => drawDropDown(d) _
      case s@Steady(top,_) => drawSteady(s) _
    }
    tr <+< draw
  }
}
