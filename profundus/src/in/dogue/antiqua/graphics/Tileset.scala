package in.dogue.antiqua.graphics

import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx.graphics.g2d.TextureRegion
import com.deweyvm.gleany.data.Recti
import com.deweyvm.gleany.AssetLoader
import in.dogue.antiqua.Implicits._
import in.dogue.antiqua.data.Array2d

case class Tileset(cols:Int, rows:Int, tileWidth:Int, tileHeight:Int, t:Texture) {
  private val regions = Array2d.tabulate(cols, rows) { case (i, j) =>
    AssetLoader.makeTextureRegion(t, Recti(i * tileWidth, j * tileHeight, tileWidth, tileHeight).some)
  }

  def getRegion(i:Int, j:Int):TextureRegion = {
    regions.get(i, j)
  }
}
