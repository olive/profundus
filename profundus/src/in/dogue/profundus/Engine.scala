package in.dogue.profundus

import in.dogue.antiqua.graphics.{TileRenderer, Renderer}
import com.deweyvm.gleany.{GleanyGame, AssetLoader}
import in.dogue.profundus.mode._
import in.dogue.profundus.input.Controls
import in.dogue.profundus.mode.Mode
import in.dogue.antiqua.graphics.Tileset
import in.dogue.profundus.mode.loadout.{LoadoutMode, Loadout}
import scala.util.Random

class Engine {
  val rows = 32 + 16
  val cols = 32
  val m = {
    if (Game.debug) {
      val x = LoadoutMode.create(cols, rows, None)//GameMode.create(cols, rows, Loadout.default, Game.getSeed)//TitleMode.create(cols, rows)//ResultMode.create(cols, rows, PlayerLog.create(Loadout.default))//StoryMode.create(cols, rows, Loadout.default)//
      x.toMode
    } else {
      TitleMode.create(cols, rows).toMode
    }
  }
  var mode:Mode[_] = m
  val ts = new Tileset(16, 16, 16, 16, AssetLoader.loadTexture("16x16"))
  val r = new Renderer(cols*16, rows*16, 1, ts)

  def update() = {
    if (Controls.Escape.justPressed) {
      GleanyGame.exit()
    }
    mode = mode.update
  }

  def draw() = {
    val tr = TileRenderer.create
    r.render(tr <+< mode.draw)
    ()
  }

}
