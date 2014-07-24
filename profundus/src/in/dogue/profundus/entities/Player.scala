package in.dogue.profundus.entities

import in.dogue.antiqua.graphics.{TileFactory, TileRenderer, Tile}
import in.dogue.antiqua.data.{Direction, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.profundus.input.Controls
import Direction.Down
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.particles.{Particle, DeathParticle}
import in.dogue.profundus.world._
import in.dogue.profundus.mode.loadout.Loadout
import in.dogue.profundus.entities.pickups._
import in.dogue.profundus.entities.pickups.Herb
import scala.util.Random
import in.dogue.profundus.lighting.LightSource
import in.dogue.profundus.ui.HudTool
import in.dogue.profundus.audio.SoundManager
import in.dogue.profundus.{Profundus, Game}
import in.dogue.profundus.entities.pickups.Herb
import in.dogue.profundus.entities.pickups.Toadstool
import in.dogue.profundus.entities.pickups.Bark
import in.dogue.profundus.world.WorldTile


object PlayerLog {
  def create(lo:Loadout) = {
    PlayerLog(lo, lo.name, DamageType.Unknown, 0, 0, lo.gems, 0, 0, 0, 0, 0, 0, Vector())
  }

}

case class PlayerLog(lo:Loadout, title:String, killedBy:DamageType, bombsUsed:Int, ropesUsed:Int, gemsCollected:Int, gemsSpent:Int, fuelUsed:Int, toolsBroken:Int, deepest:Int, timeSpent:Int, tilesDug:Int, foodEaten:Vector[FoodType]) {
  def digTile = copy(tilesDug=tilesDug+1)
  def useBomb = copy(bombsUsed = bombsUsed + 1)
  def useRope = copy(ropesUsed = ropesUsed + 1)
  def getGem = copy(gemsCollected = gemsCollected + 1)
  def spendGem = copy(gemsSpent = gemsSpent + 1)
  def useFuel = copy(fuelUsed = fuelUsed + 1)
  def breakTool = copy(toolsBroken = toolsBroken + 1)
  def setDepth(d:Int) = copy(deepest = math.max(d, deepest))
  def eatFood(food:FoodType) = copy(foodEaten=foodEaten :+ food)
  def incrTime = copy(timeSpent = timeSpent + 1)
  def getKilledBy = killedBy.message
  def timeString = {
    val mins = timeSpent/(60*60)
    val secs = timeSpent % (60*60)
    val frames = timeSpent % 60
    "%s:%02d.%02d".format(mins, (secs/60.0).toInt, ((frames/60.0) * 100).toInt)
  }
}


object Player {
  var lastHurt = 0
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
    val shovel = ToolSprite.create

    val i = ij.x
    val j = ij.y
    val smallLight = LightSource.createCircle(ij, 5, 10, 1)
    val largeLight = LightSource.createCircle(ij, 5, 13, 1)
    Player((i, j - 1), (i, j), face,
           Attributes.create, NoBuff,
           StaminaBar.create(100), HealthBar.create(200),
           shovel, getLive,
           Seq(),
           ControlState(false, false, false, false, false, false, false),
           Inventory.create(lo), PlayerLog.create(lo),
           Grounded, Alive, false,
           PlayerLight.create(smallLight, largeLight),
           0, new StepMachine)
  }
}

case class Player private (prev:(Int,Int), ij:(Int,Int), face:Direction,
                           attr:Attributes, buff:Buff,
                           stam:StaminaBar, health:HealthBar,
                           shovel:ToolSprite, t:Direction => Tile,
                           forces:Seq[Force],
                           ctrl:ControlState,
                           inv:Inventory, log:PlayerLog,
                           fall:FallState, state:LivingState, justKilled:Boolean,
                           light:PlayerLight,
                           moveT:Int, stepMachine:StepMachine) {
  @inline def x = ij.x
  @inline def y = ij.y
  def collectRope(g:RopePickup) = {
    SoundManager.item.play()
    copy(inv=inv.collectRope(g))
  }
  def collectMineral(g:MineralPickup) = {
    SoundManager.item.play()
    copy(inv=inv.collectMineral(g), log=log.getGem)
  }
  def collectFood(typ:FoodType) = {
    SoundManager.item.play()
    val newLog = log.eatFood(typ)
    val np = typ match {
      case Toadstool(seed) =>
        val regen = 1 + new Random(seed).nextInt(Attributes.default.stamRegen*2)
        val buff = ToadstoolBuff(regen, 0)
        copy(buff = buff)
      case Herb(seed) =>
        val regen = 1 + new Random(seed).nextInt(Attributes.default.healthRegen*2)
        val buff = HerbBuff(regen, 0)
        copy(buff = buff)
      case Bark(seed) =>
        val oldMax = health.max
        val newMax = oldMax + new Random(seed).nextInt(60) - 30
        copy(health=health.setMax(newMax))
    }
    np.copy(log=newLog)

  }

  def useFuel = {
    val newLight = light.useFlare
    val newInv = inv.useFlare
    copy(light=newLight, inv=newInv)
  }

  def collectTool(t:Tool) = {
    SoundManager.item.play()
    copy(inv=inv.setTool(t))
  }

  def collectItem(it:Item) = {
    SoundManager.item.play()
    copy(attr=attr.collectItem(it))
  }
  def toolPos = ((ctrl.isShovelling || Game.hasDrill) && canUseTool).select(None, ((x, y)-->face).some)
  def hasStamina = stam.amt >= inv.tool.`type`.stamCost
  def canUseTool = hasStamina

  def getStamBar = stam.vb
  def getHealthBar = health.vb
  def getBuffIcon = buff.icon
  def getItems = attr.getItems
  def pos = ij
  def hasLongArms = attr.hasLongArms
  def move(newPos:Cell, from:Direction, newTouching:Direction => Option[WorldTile]) = {
    if ((from == Direction.Left || from == Direction.Right) && newTouching(Direction.Down).exists {!_.isWalkable}) {
      stepMachine.increment()
    }
    val newP = copy(prev = ij, ij=newPos)
    if (newTouching(Direction.Down).exists {
      case WorldTile(Spike(_,_,dir,_)) if dir == Direction.Up => true
      case a => false

    }) {
      newP.kill(DamageType.Spikes.some)
    } else {
      newP
    }
  }


  def spendBomb = copy(inv = inv.spendBomb, log = log.useBomb)
  def spendRope = copy(inv = inv.spendRope, log = log.useRope)

  def setFacing(d:Direction) = (state == Dead).select(copy(face=d), this)
  def hitTool(dmg:Int, tileBroken:Boolean) = {
    if (!Game.hasDrill) {
      if (dmg > 0) {
        SoundManager.dig.play()
      } else {
        SoundManager.swish.play()
      }
    }
    val prevDur = inv.tool.dura
    val newInv1 = inv.useTool(dmg)
    val (newLog, newInv) = if (newInv1.tool.dura == 0 && prevDur > 0) {
      (log.breakTool, newInv1.setTool(BareHands.toTool))
    } else  {
      (log, newInv1)
    }
    val hurtAmt = newInv.tool.`type`.healthHurt
    if (hurtAmt > 0 && dmg > 0) {
      SoundManager.hurt.play()
    }

    val newHealth = (dmg > 0).select(health, health.permaHurt(hurtAmt))

    val newLog2 = tileBroken.select(newLog, newLog.digTile)
    val stamDmg = (dmg==0).select(inv.tool.`type`.stamCost, 0)
    val newStam = stam.remove(stamDmg)
    copy(log=newLog2, inv=newInv, stam=newStam, health=newHealth)
  }

  def chooseFace(dx:Int, dy:Int):Direction = {
    if (forces.length > 0) {
      face
    } else if (dx == Direction.Left.dx) {
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
      case Alive if forces.length == 0 => computeMove
      case _ => (this, None)
    }
  }

  private def computeMove:(Player, Option[Direction]) = {
    val dx = Controls.AxisX.isPressed
    val dy = Controls.AxisY.isPressed
    val newT = (dx !=0 || dy != 0).select(0, moveT+1)
    val moveSpeed = fall.moveSlow.select(4, 12)

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
    (dx != 0 || dy != 0).select(face, chooseFace(dx, dy))
  }

  def update: (Player, Seq[WorldSpawn]) = {
    import Profundus._
    val newAttr = buff.process(attr)
    val p = copy(ctrl=ctrl.update(canUseTool),
                 log=log.setDepth(pos.y).incrTime,
                 attr=newAttr,
                 stam=stam.update(newAttr),
                 health=health.update(newAttr),
                 light=light.update,
                 fall=if(attr.hasWings) Floating else fall)
    val (jkP, ps) = if (justKilled) {
      SoundManager.dead.play()
      (p.copy(justKilled=false), Seq(DeathParticle.create((x, y), Int.MaxValue).toParticle))
    } else {
      (p, Seq())
    }
    val fpl = if (ctrl.isFlaring && inv.hasFlare && light.lt <= 0) {
      SoundManager.flare.play()
      jkP.useFuel
    } else {
      jkP
    }
    (fpl, ps.gss)
  }

  def updateDropTool(p:Player, tc:TerrainCache):(Player, Seq[Pickup]) = {
    if (ctrl.isDropping && !inv.tool.isBare && face.isHorizontal && !tc.isSolid(pos --> face.opposite)) {
      SoundManager.drop.play()
      val newInv = inv.setTool(BareHands.toTool)
      val pickup = ToolPickup.create(pos --> face.opposite, inv.tool)
      (p.copy(inv=newInv), Seq(pickup))
    } else {
      (p, Seq())
    }
  }

  def fallDamage(fall:Int):Option[Damage] = {
    val d = attr.fallDistance
    val amount = if (fall < d) {
      None
    } else {
      (100*((fall - d)/d.toDouble)).toInt.some
    }
    amount.map{a => Damage(a, DamageType.Fall)}
  }

  def setFallState(s:FallState) = {
    val newPl = copy(fall=s)
    (fall, s) match {
      case (Falling(_, num), Grounded)  =>
        if (num > 1) {
          SoundManager.land.play()
        }
        val dmg = fallDamage(num)
        dmg.map { value => newPl.damage(value)}.getOrElse(newPl)

      case _ => newPl
    }
  }

  def damage(dmg:Damage):Player = {
    if (state == Alive && dmg.amount > 0 && Game.t - Player.lastHurt > 7) {
      Player.lastHurt = Game.t
      SoundManager.hurt.play()
    }
    val newHealth = if (dmg.amount > 0) {
      health.remove(dmg)
    } else {
      health
    }
    val f = if (newHealth.amt <= 0) {
      (p:Player) => p.kill(None)
    } else {
      id[Player] _
    }

    f(this.copy(health=newHealth))
  }

  def kill(dmg:Option[DamageType]):Player = {
    val deathCause = dmg.getOrElse(health.last)
    if (attr.hasHalo) {
      this
    } else {
      copy(state=Dead,
           health=health.removeAll,
           log=log.copy(killedBy=deathCause),
           face=Direction.Down,
           t=Player.getDead,
           justKilled=state!=Dead,
           buff=DeadBuff)
    }

  }

  private def drawShovel(tr:TileRenderer):TileRenderer = {
    ctrl.isShovelling.select(
      tr,
      tr <+< shovel.draw(face)(ij)
    )
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (ij, t(face)) <+< drawShovel
  }

  def processForces(tc:TerrainCache) = {
    val newFs = if (ctrl.isKicking && tc.isSolid(pos --> face)) {
      Seq(Force.constForce(2, 15, face.opposite, 0))
    } else {
      Seq()
    }
    val fs = if (fall == Grounded) {
      Seq()
    } else {
      forces.filter{!_.isDone} ++ newFs
    }
    val (newPl, newForces) = fs.foldLeft((this, Seq[Force]())) { case ((pl, forces), force) =>
      force.affect(pl.pos, tc).map { case (newPos, dir) =>
        if (tc.isSolid(newPos)) {
          pl @@ forces
        } else {
          pl.resetFall.move(newPos, dir, tc.getTouching(newPos)) @@ (force +: forces)
        }
      }.getOrElse((pl, force +: forces))
    }
    newPl.copy(forces=newForces.map{_.update})
  }

  def resetFall = copy(fall=Grounded)


  def toMassive:Massive[Player] = Massive(_.pos, _.move, _.setFallState, fall, this)
  def toLight:LightSource = light.toLightSource(pos)
}
