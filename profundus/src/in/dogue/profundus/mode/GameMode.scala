package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.world.{World, TerrainManager}
import scala.util.Random
import in.dogue.profundus.entities.{Alive, Dead, Player}
import in.dogue.profundus.ui.Hud
import in.dogue.antiqua.Implicits
import Implicits._

object GameMode {
  def create(cols:Int, rows:Int, i:Int, j:Int) = {
    val r = new Random(0)
    val hudHeight = 4
    val w = World.create(cols, rows-hudHeight, r)
    val pl = Player.create(10,10)
    val hud = Hud.create(cols, hudHeight)
    GameMode(cols, rows, pl, w, new TerrainManager(), hud, r)
  }
}

case class GameMode private(cols:Int, rows:Int, pl:Player, w:World, mgr:TerrainManager, hud:Hud, r:Random) {

  def update = {
    val (newW, newPl) = mgr.update(w, pl)
    val explored = newW.checkPositions(newPl.pos)
    val newHud = hud.atDepth(pl.pos.y)
    newPl.state match {
      case Dead => TitleMode.create(cols, rows).toMode
      case Alive => copy(pl=newPl, w=explored, hud=newHud).toMode
    }

  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr.withMove(0, hud.height){ t =>
      t.<+<(w.draw(pl.pos)).<+<(pl.draw)
    }.<+<(hud.draw)
  }

  def toMode:Mode[GameMode] = Mode(_.update, _.draw, this)
}
