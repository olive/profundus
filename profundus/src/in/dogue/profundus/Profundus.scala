package in.dogue.profundus

import in.dogue.antiqua.graphics.TileRenderer

object Profundus {
  implicit def tileRenderer2Aug(tr:TileRenderer) = new AugTileRenderer(tr)
}
