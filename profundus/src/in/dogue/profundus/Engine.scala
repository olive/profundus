package in.dogue.profundus

import in.dogue.antiqua.graphics.{Tileset, TileRenderer, Renderer}
import com.deweyvm.gleany.{GleanyGame, AssetLoader}
import in.dogue.profundus.mode.{Mode, GameMode}
import in.dogue.profundus.input.Controls

class Engine {
  var mode:Mode[_] = GameMode.create(32, 32).toMode//TitleMode.create(32, 32).toMode//
  val ts = new Tileset(16, 16, 16, 16, AssetLoader.loadTexture("16x16"))
  val r = new Renderer(512, 512, 1, ts)

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
