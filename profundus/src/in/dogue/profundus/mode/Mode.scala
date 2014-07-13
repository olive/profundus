package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.TileRenderer



case class Mode[T](up:T => Mode[_],
                   dr:T => (TileRenderer) => TileRenderer,
                   self:T) {
  def update:Mode[_] = {
    up(self)
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< dr(self)
  }
}
