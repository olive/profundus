package in.dogue.profundus.doodads

import in.dogue.antiqua.graphics.TileRenderer

case class Doodad[T](up: T => T,
                     dr: T => TileRenderer => TileRenderer,
                     self: T) {
  def update = copy(self=up(self))
  def draw(tr:TileRenderer):TileRenderer = dr(self)(tr)
}
