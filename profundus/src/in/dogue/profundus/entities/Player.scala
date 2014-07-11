package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileFactory, TileRenderer, Tile}
import in.dogue.antiqua.data.{Direction, Code}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.input.Controls
import Direction.Down
import in.dogue.antiqua.Implicits
import Implicits._


sealed trait PlayerState
case object Alive extends PlayerState
case object Dead extends PlayerState


object Player {
  def create(i:Int, j:Int) = {
    val shovel = Shovel.create
    val f = TileFactory(Color.Black, Color.White)
    def getTile(d:Direction) = {
      val code = d match {
        case Direction.Up => Code.▀
        case Direction.Down => Code.▄
        case Direction.Left => Code.▌
        case Direction.Right => Code.▐
      }
      f(code)
    }
    Player(i, j - 1, i, j, Down, shovel, getTile, false, false, 0, Alive)
  }
}

case class Player private (prevX:Int, prevY:Int, x:Int, y:Int,
                           face:Direction, shovel:Shovel, t:Direction => Tile,
                           isShovelling:Boolean, isClimbing:Boolean, fallFrames:Int,
                           state:PlayerState) {
  def shovelPos = isShovelling.select(None, ((x, y)-->face).some)
  def pos = (x, y)
  def move(newPos:(Int,Int)) = {
    copy(prevX = x, prevY = y, x=newPos._1, y=newPos._2)
  }

  def land = {
    val newState = if(fallFrames > 7) {
      Dead
    } else {
      state
    }

    copy(fallFrames=0, state=newState)
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
    copy(isShovelling=Controls.Space.isPressed,
         isClimbing=Controls.Action.justPressed)
  }

  private def drawShovel(tr:TileRenderer):TileRenderer = {
    isShovelling.select(
      tr,
      tr <+< shovel.draw(face)(x, 14-28 - 5)
    )
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (x, 14-28 - 5, t(face)) <+< drawShovel
  }
}
