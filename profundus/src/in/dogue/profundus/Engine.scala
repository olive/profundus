package in.dogue.profundus

import in.dogue.antiqua.graphics.{Tileset, TileRenderer, Renderer}
import com.deweyvm.gleany.AssetLoader
import in.dogue.profundus.mode.{Mode, GameMode}

class Engine {
  var mode:Mode[_] = GameMode.create(32, 32, 16,16).toMode
  val ts = new Tileset(16, 16, 16, 16, AssetLoader.loadTexture("Md_curses_16x16"))
  val r = new Renderer(512, 512, 1, ts)

  def update() = {
    mode = mode.update
  }

  def draw() = {
    val tr = TileRenderer.create
    r.render(tr <+< mode.draw)
    ()
  }

}
