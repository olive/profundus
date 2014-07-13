package in.dogue.profundus

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.graphics.AugTileRenderer

object Profundus {
  implicit def tileRenderer2Aug(tr:TileRenderer) = new AugTileRenderer(tr)
}
