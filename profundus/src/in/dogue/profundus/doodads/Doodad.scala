package in.dogue.profundus.doodads

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.lighting.LightSource


object Doodad {
  def apply[A](aup: A => A,
               adr: A => TileRenderer => TileRenderer,
               alight:A => Option[LightSource],
               aself: A) = new Doodad {
    override type T = A
    override val self = aself
    override val up = aup
    override val light = alight
    override val dr = adr
  }
}

trait Doodad {
  type T
  val up: T => T
  val dr: T => TileRenderer => TileRenderer
  val light:T => Option[LightSource]
  val self: T
  def update = Doodad(up, dr, light, up(self))
  def getLight = light(self)
  def draw(tr:TileRenderer):TileRenderer = dr(self)(tr)
}
