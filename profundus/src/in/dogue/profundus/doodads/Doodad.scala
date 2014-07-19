package in.dogue.profundus.doodads

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.lighting.LightSource

case class Doodad[T](up: T => T,
                     dr: T => TileRenderer => TileRenderer,
                     light:T => Option[LightSource],
                     self: T) {
  def update = copy(self=up(self))
  def getLight = light(self)
  def draw(tr:TileRenderer):TileRenderer = dr(self)(tr)
}
