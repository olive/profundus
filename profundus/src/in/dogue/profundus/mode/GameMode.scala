package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.world.{World, TerrainManager}
import scala.util.Random
import in.dogue.profundus.entities.{Alive, Dead, Player}
import in.dogue.profundus.ui.Hud
import in.dogue.antiqua.Implicits
import Implicits._
import in.dogue.profundus.particles.ParticleManager

object GameMode {
  def create(cols:Int, rows:Int, i:Int, j:Int) = {
    val r = new Random(0)
    val hudHeight = 4
    val w = World.create(cols, rows-hudHeight, r)
    val pl = Player.create(10,10)
    val hud = Hud.create(cols, hudHeight)
    GameMode(cols, rows, pl, w, new TerrainManager(), ParticleManager.create, hud, r)
  }
}

case class GameMode private(cols:Int, rows:Int, pl:Player, w:World, mgr:TerrainManager, pm:ParticleManager, hud:Hud, r:Random) {

  def update = {
    val inserted = updateItemUse(w, pl)
    val (newW, newPl) = mgr.update(inserted, pl)
    val (explored, ps) = newW.update(newPl.pos)
    val newHud = hud.atDepth(pl.pos.y)
    val newPm = pm.update
    newPl.state match {
      case Dead => TitleMode.create(cols, rows).toMode
      case Alive =>
        copy(pl=newPl, w=explored, hud=newHud, pm=newPm ++ ps).toMode
    }
  }

  private def updateItemUse(w:World, p:Player) = {
    if (p.isBombing) {
      w.insertBomb(p.pos --> p.face)
    } else {
      w
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
