package in.dogue.profundus

import in.dogue.antiqua.graphics.{TileRenderer, Renderer}
import com.deweyvm.gleany.{GleanyGame, AssetLoader}
import in.dogue.profundus.mode._
import in.dogue.profundus.input.Controls
import in.dogue.profundus.mode.Mode
import in.dogue.antiqua.graphics.Tileset
import in.dogue.profundus.mode.loadout.{LoadoutMode, Loadout}
import scala.util.Random
object Engine {
  var t = 0.0
  var tn = 0
  var ms = 0.0
}
class Engine {
  import Engine._
  val cols = 32
  val rows = 32 + 16
  val m = {

    if (Game.debug) {

      val x = GameMode.create(cols, rows, Loadout.default, Game.getSeed)//LoadoutMode.create(cols, rows, None)//TitleMode.create(cols, rows)//ResultMode.create(cols, rows, PlayerLog.create(Loadout.default))//StoryMode.create(cols, rows, Loadout.default)//
      x.toMode
    } else {
      TitleMode.create(cols, rows).toMode
    }
  }
  var mode:Mode[_] = m
  val ts = new Tileset(16, 16, 16, 16, AssetLoader.loadTexture("16x16"))
  val r = new Renderer(cols*16, rows*16, 1, ts)

  def update() = {
    if (Controls.Debug.justPressed) {
      Game.lightsOff = !Game.lightsOff
    }
    if (Controls.Escape.justPressed) {
      GleanyGame.exit()
    }
    mode = mode.update
  }

  def draw() = {
    val tr = TileRenderer.create(cols, rows)
    val time = System.nanoTime
    val tt = tr <+< mode.draw
    val amt = (System.nanoTime - time)/1000000.0
    t += amt
    tn += 1
    if (tn % 30 == 0) {
      ms = t/30.0
      t=0
      tn=0
    }
    r.render(tt)
    ()
  }

}
