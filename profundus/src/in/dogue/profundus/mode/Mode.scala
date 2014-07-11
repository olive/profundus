package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer

case class Mode[T](up:T => T,
                   draw:T => (TileRenderer) => TileRenderer,
                   self:T) {
  def update = copy(self=up(self))
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< draw(self)
  }
}
