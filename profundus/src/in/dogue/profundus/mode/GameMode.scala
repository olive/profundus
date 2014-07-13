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
  def create(cols:Int, rows:Int, i:Int, j:Int) = {
    val r = new Random(0)
    val hudHeight = 4
    val w = World.create(cols, rows-hudHeight, r)
    val pl = Player.create(10,10)
    val hud = Hud.create(cols, hudHeight, pl.inv)
    GameMode(cols, rows, pl, w, new TerrainManager(), ParticleManager.create, hud, r)
  }
}

case class GameMode private(cols:Int, rows:Int, pl:Player, w:World, mgr:TerrainManager, pm:ParticleManager, hud:Hud, r:Random) {

  def update = {
    val climbPl = updateClimbRope(w, pl)
    val (insertedW, bombedPl) = updateItemUse(w, climbPl)
    val (strippedW, collectedPl) = insertedW.collectGems(bombedPl)
    val (newW, newPl) = mgr.update(strippedW, collectedPl)
    val (explored, ps) = newW.update(newPl.pos)
    val newHud = hud.atDepth(pl.pos.y).withInventory(pl.inv)
    val newPm = pm.update
    newPl.state match {
      case Dead => TitleMode.create(cols, rows).toMode
      case Alive =>
        copy(pl=newPl, w=explored, hud=newHud, pm=newPm ++ ps).toMode
    }
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
    tr.withMove(0, hud.height){ t =>
      t.withMove(0, -pl.y - 5 - 16) { worldPos =>
        worldPos <+< w.draw(pl.pos)  <+< pl.draw <+< pm.draw
      }
    }.<+<(hud.draw)
  }

  def toMode:Mode[GameMode] = Mode(_.update, _.draw, this)
}
