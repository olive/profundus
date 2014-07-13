package in.dogue.profundus.graphics

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.data.Array2d

class AugTileRenderer(tr:TileRenderer) {

  def <+++<[T](draws:Array2d[T], f:T => Tile, g:Int => Int):TileRenderer = {
    tr <+++ draws.map { case (i, j, t) => f(t)}
  }
}
