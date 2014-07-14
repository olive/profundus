package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.world.{World, TerrainManager}
import scala.util.Random
import in.dogue.profundus.entities._
import in.dogue.profundus.ui.Hud
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.particles.ParticleManager

object GameMode {
  def create(cols:Int, rows:Int, lo:Loadout) = {
    val r = new Random(0)
    val hudHeight = 5
    val (w,p) = World.create(cols, rows-hudHeight, r)
    val pl = Player.create(p, lo)
    val (newWorld, _) = w.update(pl.pos)
    val hud = Hud.create(cols, hudHeight, pl.inv)
    GameMode(cols, rows, pl, newWorld, new TerrainManager(), ParticleManager.create, hud, r)
  }
}

case class GameMode private(cols:Int, rows:Int, pl:Player, w:World, mgr:TerrainManager, pm:ParticleManager, hud:Hud, r:Random) {

  def update = {
    val updated = selfUpdate
    updated.pl.state match {
      case Dead => DeadMode.create(cols, rows, this, pl.log.lo).toMode
      case Alive => updated.toMode
    }
  }

  def selfUpdate:GameMode = {
    val climbPl = updateClimbRope(w, pl)
    val (insertedW, bombedPl) = updateItemUse(w, climbPl)
    val (strippedW, collectedPl) = insertedW.collectGems(bombedPl)
    val (newW, newPl) = mgr.update(strippedW, collectedPl)
    val (explored, ps) = newW.update(newPl.pos)
    val newHud = hud.atDepth(pl.pos.y).withInventory(pl.inv)
    val newPm = pm.update
    val (newEs, killed) = explored.killEntities(newPl)
    val esWorld = explored.copy(es=newEs)
    copy(pl=killed, w=esWorld, hud=newHud, pm=newPm ++ ps)
  }

  private def updateClimbRope(w:World, p:Player):Player = {
    val curState = p.fall
    if (w.isRope(p.pos)) {
      p.setFallState(Floating)
    } else {
      p.setFallState(curState match {
        case Floating => Falling.create
        case s => s
      })

    }
  }

  private def updateItemUse(w:World, p:Player):(World, Player) = {
    if (p.isBombing && !p.isRoping && p.inv.hasBomb) {
      (w.insertBomb(p.pos --> p.face), p.spendBomb)
    } else if (p.isRoping && p.inv.hasRope) {
      (w.insertRope(p.pos), p.spendRope)
    } else {
      (w, p)
    }

  }
  //world tiles are all on 0,0 so they must be adjusted by the players position, whereas the players position is absolute
  def draw(tr:TileRenderer):TileRenderer = {
    val offset = 0//5
    tr.withMove(0, hud.height){ t =>
      t.withMove(0, -pl.y - offset - 16) { worldPos =>
        worldPos <+< w.draw(pl.pos)  <+< pl.draw <+< pm.draw
      }
    }.<+<(hud.draw)
  }

  def toMode:Mode[GameMode] = Mode(_.update, _.draw, this)
}
