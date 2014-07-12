package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileFactory, TileRenderer, Tile}
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.input.Controls
import Direction.Down
import in.dogue.antiqua.Implicits
import Implicits._


sealed trait LivingState
case object Alive extends LivingState
case object Dead extends LivingState


sealed trait FallState {
  val tiles:Int
}
case class Falling(t:Int, override val tiles:Int) extends FallState {
  val fallTime = 6
}
case object Grounded extends FallState {
  override val tiles = 0
}

object Player {
  def create(i:Int, j:Int) = {
    val shovel = Shovel.create
    def getTile(d:Direction) = {
      val code = d match {
        case Direction.Up => CP437.▀
        case Direction.Down => CP437.▄
        case Direction.Left => CP437.▌
        case Direction.Right => CP437.▐
      }
      code.mkTile(Color.Black, Color.White)
    }
    Player(i, j - 1, i, j, Down,
           shovel, getTile,
           false, false, false,
           Inventory.create,
           Grounded, Alive)
  }
}

case class Player private (prevX:Int, prevY:Int, x:Int, y:Int, face:Direction,
                           shovel:Shovel, t:Direction => Tile,
                           isShovelling:Boolean, isClimbing:Boolean, isBombing:Boolean,
                           inv:Inventory,
                           fall:FallState, state:LivingState) {

  def collect(g:MineralDrop) = copy(inv=inv.collect(g))
  def shovelPos = isShovelling.select(None, ((x, y)-->face).some)
  def pos = (x, y)
  def move(newPos:(Int,Int)) = {
    copy(prevX = x, prevY = y, x=newPos._1, y=newPos._2)
  }

  def spendBomb = copy(inv = inv.spendBomb)

  def land = {
    val newState = if (fall.tiles > 17) {
      Dead
    } else {
      state
    }

    copy(fall = Grounded, state=newState)
  }

  def setFacing(d:Direction) = copy(face=d)


  private def chooseFace(dx:Int, dy:Int):Direction = {
    if (dx == Direction.Left.dx) {
      Direction.Left
    } else if (dx == Direction.Right.dx) {
      Direction.Right
    } else if (dy == Direction.Up.dy) {
      Direction.Up
    } else {
      Direction.Down
    }

  }

  def getMove:Option[Direction] = {
    val dx = Controls.AxisX.zip(5,5)
    val dy = Controls.AxisY.zip(5,5)
    if (dx != 0 || dy != 0) {
      chooseFace(dx, dy).some
    } else {
      None
    }
  }

  def update = {
    copy(isShovelling=Controls.Space.justPressed,
         isClimbing=Controls.Action.justPressed,
         isBombing=Controls.Capsule.justPressed)
  }

  private def setFallState(s:FallState) = {
    copy(fall=s)
  }

  private def drawShovel(tr:TileRenderer):TileRenderer = {
    isShovelling.select(
      tr,
      tr <+< shovel.draw(face)(x, y)
    )
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (x, y, t(face)) <+< drawShovel
  }


  def toMassive:Massive[Player] = Massive(_.pos, _.move, _.setFallState, fall, this)
}
