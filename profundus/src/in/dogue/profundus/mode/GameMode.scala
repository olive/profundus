package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.world.{World, TerrainManager}
import scala.util.Random
import in.dogue.profundus.entities._
import in.dogue.profundus.ui.Hud
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.particles.ParticleManager
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.experimental.GreatWorld

object GameMode {
  def create(cols:Int, rows:Int, lo:Loadout) = {
    val r = new Random(0)
    val hudHeight = 5
    //val (w,p) = World.create(cols, rows-hudHeight, r)
    //val pl = Player.create(p, lo)
    //val (newWorld, _) = w.update(pl.pos, pl.state)
    val gw = GreatWorld.create(cols, rows - hudHeight, lo, r)
    val hud = Hud.create(cols, hudHeight, gw.p.inv)
    GameMode(cols, rows, gw, hud, r)
  }
}

case class GameMode private(cols:Int, rows:Int, gw:GreatWorld, hud:Hud, r:Random) {

  def update = {
    val updated = selfUpdate
    updated.gw.p.state match {
      case Dead => DeadMode.create(cols, rows, this, updated.gw.p.log.lo).toMode
      case Alive => updated.toMode
    }
  }

  def selfUpdate:GameMode = {
    copy(gw=gw.update)
  }


  //world tiles are all on 0,0 so they must be adjusted by the players position, whereas the players position is absolute
  def draw(tr:TileRenderer):TileRenderer = {
    tr.withMove(0, hud.height){ t =>
      t <+< gw.draw
    }.<+<(hud.draw)
  }

  def toMode:Mode[GameMode] = Mode(_.update, _.draw, this)
}
