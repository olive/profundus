package in.dogue.profundus

import in.dogue.antiqua.graphics.{Tileset, TileRenderer, Renderer}
import com.deweyvm.gleany.AssetLoader

class Engine {
  var exampleMode = ExampleMode.create(16,16)
  val ts = new Tileset(16, 16, 16, 16, AssetLoader.loadTexture("Md_curses_16x16"))
  val r = new Renderer(512, 512, 1, ts)

  def update() = {
    exampleMode = exampleMode.update
  }
  def draw() = {
    val tr = TileRenderer.create
    r.render(tr <+< exampleMode.draw)
    ()
  }

}
