package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.world.World

sealed trait RopeState {
  val maxFly = 6
}
object FlyUp { def create = FlyUp(0,0)}
case class FlyUp(t:Int, amt:Int) extends RopeState {
  val flySpeed = 6
}
object RollDown { def create(len:Int) = RollDown(len, 0,0)}
case class RollDown(len:Int, t:Int, amt:Int) extends RopeState {
  val flySpeed = 6
}
case class Steady(len:Int) extends RopeState

object Rope {
  def create(ij:(Int,Int)) = Rope(ij, FlyUp.create)
}

case class Rope private (bottom:(Int,Int), state:RopeState) {
  def update(w:World):Rope = {
    state match {
      case f@FlyUp(t, amt) => updateFlyUp(w, f, t, amt)
      case r@RollDown(_, t, amt) => updateRollDown(r, t, amt)
      case a => this
    }
  }

  private def updateRollDown(r:RollDown, t:Int, amt:Int) = {
    val newT = t + 1
    val raised = if (newT % r.flySpeed == 0) {
      copy(state=RollDown(r.len, 0, amt+1))
    } else {
      copy(state=RollDown(r.len, newT, amt))
    }

    if (amt == r.maxFly) {
      copy(state=Steady(r.len))
    } else {
      raised
    }

  }

  private def updateFlyUp(w:World, f:FlyUp, t:Int, amt:Int) = {
    val newT = t + 1
    val raised = if (newT % f.flySpeed == 0) {
      copy(state=FlyUp(0, amt+1))
    } else {
      copy(state=FlyUp(newT, amt))
    }

    if (w.isSolid(bottom -| amt --> Direction.Up) || amt == f.maxFly) {
      val len = scala.math.min(amt + 1, f.maxFly)
      copy(state=RollDown.create(len))
    } else {
      raised
    }

  }

  def ropePos(ij:(Int,Int)) = {
    state match {
      case Steady(len) =>
        val (x, y1) = bottom
        val y2 = bottom.y - len
        ij.x == x && ij.y <= y1 && ij.y >= y2
      case _ => false
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    state match {
      case FlyUp(_, amt) =>
        val (x, y) = bottom -| amt
        tr <| (x, y, CP437.°.mkTile(Color.Black, Color.White))
      case r@RollDown(len, _, amt) =>
        val (x, y) = bottom -| r.len
        val draws = (0 until (amt-1)) map { k =>
          (x, y + k +1, CP437.│.mkTile(Color.Black, Color.White))
        }
        tr <|| draws
      case p@Steady(len) =>
        val (x, y) = bottom
        val draws = (0 until len) map { k =>
          (x, y - k, CP437.│.mkTile(Color.Black, Color.White))
        }
        tr <|| draws
    }
  }
}
