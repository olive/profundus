package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileFactory, TileRenderer, Tile}
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.input.Controls
import Direction.Down
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.particles.{Particle, DeathParticle}
import in.dogue.profundus.world.{Spike, WorldTile}
import in.dogue.profundus.mode.loadout.Loadout
import in.dogue.profundus.Game


object PlayerLog {
  def create(lo:Loadout) = {
    PlayerLog(lo, "name", "title", "killedby", 0, 0, lo.gems, 0, 0, 0, 0, 0, 0)
  }

}

case class PlayerLog(lo:Loadout, name:String, title:String, killedBy:String, bombsUsed:Int, ropesUsed:Int, gemsCollected:Int, gemsSpent:Int, fuelUsed:Int, toolsBroken:Int, deepest:Int, timeSpent:Int, tilesDug:Int) {
  def digTile = copy(tilesDug=tilesDug+1)
  def useBomb = copy(bombsUsed = bombsUsed + 1)
  def useRope = copy(ropesUsed = ropesUsed + 1)
  def getGem = copy(gemsCollected = gemsCollected + 1)
  def spendGem = copy(gemsSpent = gemsSpent + 1)
  def useFuel = copy(fuelUsed = fuelUsed + 1)
  def breakTool = copy(toolsBroken = toolsBroken + 1)
  def setDepth(d:Int) = copy(deepest = math.max(d, deepest))
  def incrTime = copy(timeSpent = timeSpent + 1)
}


object Player {

  def getLive(d:Direction) = {
    val code = d match {
      case Direction.Up => CP437.▀
      case Direction.Down => CP437.▄
      case Direction.Left => CP437.▌
      case Direction.Right => CP437.▐
    }
    code.mkTile(Color.Black, Color.Purple)
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
  def create(ij:Cell, face:Direction, lo:Loadout) = {
    val shovel = ShovelSprite.create

    val i = ij.x
    val j = ij.y
    Player(i, j - 1, i, j, face,
           shovel, getLive,
           false, false, false, false,
           Inventory.create(lo), PlayerLog.create(lo),
           Grounded, Alive, false,
           0)
  }
}

case class Player private (prevX:Int, prevY:Int, x:Int, y:Int, face:Direction,
                           shovel:ShovelSprite, t:Direction => Tile,
                           isShovelling:Boolean, isClimbing:Boolean, isBombing:Boolean, isRoping:Boolean,
                           inv:Inventory, log:PlayerLog,
                           fall:FallState, state:LivingState, justKilled:Boolean,
                           moveT:Int) {

  def collectRope(g:RopePickup) = copy(inv=inv.collectRope(g))
  def collectMineral(g:MineralPickup) = copy(inv=inv.collectMineral(g), log=log.getGem)
  def shovelPos = (isShovelling && inv.hasShovelUse).select(None, ((x, y)-->face).some)
  def pos = (x, y)
  def move(newPos:Cell, from:Direction, newTouching:Direction => Option[WorldTile]) = {
    val newP = copy(prevX = x, prevY = y, x=newPos._1, y=newPos._2)
    if (newTouching(Direction.Down).exists {
      case WorldTile(Spike(_,_,dir,_)) => true
      case a => false

    }) {
      newP.kill
    } else {
      newP
    }
  }


  def spendBomb = copy(inv = inv.spendBomb, log = log.useBomb)
  def spendRope = copy(inv = inv.spendRope, log = log.useRope)

  def setFacing(d:Direction) = (state == Dead).select(copy(face=d), this)
  def hitTool(dmg:Int, broken:Boolean) = {
    val prevDur = inv.tool.dura
    val newInv = inv.useTool(dmg)
    val newLog = if (newInv.tool.dura == 0 && prevDur > 0) {
      log.breakTool
    } else {
      log
    }
    val newLog2 = broken.select(newLog, newLog.digTile)
    copy(log=newLog2, inv=newInv)
  }

  def chooseFace(dx:Int, dy:Int):Direction = {
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

  def getMove:(Player, Option[Direction]) = {
    state match {
      case Alive => computeMove
      case Dead => (this, None)
    }
  }

  private def computeMove:(Player, Option[Direction]) = {
    val dx = Controls.AxisX.isPressed
    val dy = Controls.AxisY.isPressed
    val newT = (dx !=0 || dy != 0).select(0, moveT+1)
    val moveSpeed = fall.moveSlow.select(4, 8)

    val face = if (newT > 0 && newT % moveSpeed == 0) {
      chooseFace(dx, dy).some
    } else {
      None
    }

    (copy(moveT=newT), face)

  }
  /** instantaneous direction */
  def instDir = {
    val dx = Controls.AxisX.isPressed
    val dy = Controls.AxisY.isPressed
    (dx !=0 || dy != 0).select(face, chooseFace(dx, dy))
  }

  def update = {

    val newP = copy(isShovelling=Controls.Space.justPressed,
                    isClimbing=Controls.Action.justPressed,
                    isBombing=Controls.Capsule.justPressed,
                    isRoping=Controls.Rope.justPressed,
                    log=log.setDepth(pos.y).incrTime)
    if (justKilled) {
      (newP.copy(justKilled=false), Seq(DeathParticle.create(x, y, Int.MaxValue).toParticle))
    } else {
      (newP, Seq())
    }
  }

  def setFallState(s:FallState) = {
    val newPl = copy(fall=s)
    (fall, s) match {
      case (Falling(_, num), Grounded) if num > 20 =>
        newPl.kill
      case _ => newPl
    }
  }

  def kill:Player = {
    copy(state=Dead, face = Direction.Down, t=Player.getDead, justKilled=state != Dead)
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
