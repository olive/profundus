package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileFactory, Tile, TileRenderer}
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.world.TerrainCache
import in.dogue.antiqua.algebra.Monoid

sealed trait RopeState {
  val throwHeight = 6
  val maxLength = 8
  val dropSpeed = 5
  val flySpeed = 5
}

object FlyUp { def create(src:Cell) = FlyUp(src, 0, 0) }
case class FlyUp private (private val src:Cell, len:Int, t:Int) extends RopeState {
  def incrLen = copy(len=len+1, t=0)
  def x = src.x
  def y = src.y
  def top = src -| len
}

object DropDown { def create(top:Cell) = DropDown(top, 0, 0) }
case class DropDown private (private val top:Cell, len:Int, t:Int) extends RopeState {
  def incrLen = copy(len=len+1, t=0)
  def x = top.x
  def y = top.y
  def bot = top +| len
}

object Steady { def create(top:Cell, len:Int) = Steady(top, len) }
case class Steady private (top:Cell, len:Int) extends RopeState {
  def x = top.x
  def y = top.y
}

object Rope {
  private val tf = TileFactory(Color.Black, Color.White)
  private val nub = tf(CP437.a)
  private val top = tf(CP437.⌠)
  private val mid = tf(CP437.│)
  private val bot = tf(CP437.⌡)
  def create(state:RopeState) = {
    Rope(state, nub, top, mid, bot, Alive)
  }
}

case class Rope private (state:RopeState, nubT:Tile, topT:Tile, midT:Tile, bottomT:Tile, live:LivingState) {

  /** y2 > y1 */
  private def between(ij:Cell, x:Int, y1:Int, y2:Int) = ij.x == x && ij.y >= y1 && ij.y <= y2

  def ropeContains(ij:Cell) = state match {
    case FlyUp(_, _, _) => false
    case DropDown(top, len, _) => between(ij, top.x, top.y, top.y + len)
    case Steady(top, len) => between(ij, top.x, top.y, top.y + len)
  }

  def update(tc:TerrainCache):(Option[Rope], Seq[Pickup[_]]) = {
    val (rs, picks) = state match {
      case f@FlyUp(_,_,_) => updateFlyUp(f, tc)
      case d@DropDown(_,_,_) => (updateDropDown(d, tc).some, Seq())
      case s@Steady(_,_) => updateSteady(s)
    }
    (rs.map { s => copy(state=s) }, picks)
  }

  private def updateFlyUp(f:FlyUp, tc:TerrainCache):(Option[RopeState], Seq[Pickup[_]]) = {
    val newT = f.t + 1
    if (newT % f.flySpeed == 0) {
      val top = f.top
      if (tc.isSolid(top --> Direction.Up) || f.len + 1 == f.maxLength) {
        if (tc.isBackgroundSolid(top)) {
          (DropDown.create(top).some, Seq())
        } else {
          (None, Seq(RopePickup.create(top).toPickup))
        }

      } else {
        (f.incrLen.some, Seq())
      }
    } else {
      (f.copy(t=newT).some, Seq())
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


  def isKillableAt(p:Cell) = {
    ropeContains(p)
  }
  def kill = copy(live=Dead)

  private def updateSteady(s:Steady) = {
    if (live == Dead) {
      (None, Seq(RopePickup.create(s.top).toPickup))
    } else {
      (s.some, Seq())
    }

  }


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
