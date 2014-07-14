package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileFactory, TileRenderer, Tile}
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.input.Controls
import Direction.Down
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.mode.Loadout


sealed trait LivingState
case object Alive extends LivingState
case object Dead extends LivingState





object Player {

  def getLive(d:Direction) = {
    val code = d match {
      case Direction.Up => CP437.▀
      case Direction.Down => CP437.▄
      case Direction.Left => CP437.▌
      case Direction.Right => CP437.▐
    }
    code.mkTile(Color.Black, Color.White)
  }

  def getDead(d:Direction) = {
    val code = d match {
      case Direction.Up => CP437.▀
      case Direction.Down => CP437.▄
      case Direction.Left => CP437.▌
      case Direction.Right => CP437.▐
    }
    code.mkTile(Color.Black, Color.Red.dim(2))
  }
  def create(ij:(Int,Int), lo:Loadout) = {
    val shovel = ShovelSprite.create

    val i = ij.x
    val j = ij.y
    Player(i, j - 1, i, j, Down,
           shovel, getLive,
           false, false, false, false,
           Inventory.create(lo),
           Grounded, Alive)
  }
}

case class Player private (prevX:Int, prevY:Int, x:Int, y:Int, face:Direction,
                           shovel:ShovelSprite, t:Direction => Tile,
                           isShovelling:Boolean, isClimbing:Boolean, isBombing:Boolean, isRoping:Boolean,
                           inv:Inventory,
                           fall:FallState, state:LivingState) {

  def collect(g:MineralDrop) = copy(inv=inv.collect(g))
  def shovelPos = (isShovelling && inv.hasShovelUse).select(None, ((x, y)-->face).some)
  def pos = (x, y)
  def move(newPos:(Int,Int)) = {
    copy(prevX = x, prevY = y, x=newPos._1, y=newPos._2)
  }

  def spendBomb = copy(inv = inv.spendBomb)
  def spendRope = copy(inv = inv.spendRope)

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
    state match {
      case Alive => computeMove
      case Dead => None
    }


  }

  private def computeMove:Option[Direction] = {
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
         isBombing=Controls.Capsule.justPressed,
         isRoping=Controls.Capsule.justPressed && Controls.Up.isPressed)
  }

  def setFallState(s:FallState) = {
    val newPl = copy(fall=s)
    (fall, s) match {
      case (Falling(_, num), Grounded) if num > 6=>
        newPl.kill
      case _ => newPl
    }
  }

  def kill = copy(state=Dead, face = Direction.Down, t=Player.getDead)

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
