package in.dogue.profundus.doodads

import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.profundus.lighting.LightSource
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.profundus.world.Unloadable


object Doodad {
  def apply[A](aup: A => A,
               adr: A => TileRenderer => TileRenderer,
               alight:A => Option[LightSource],
               apos: A => Cell,
               aself: A) = new Doodad {
    override type T = A
    override val self = aself
    override val up = aup
    override val light = alight
    override val pos = apos
    override val dr = adr
  }
}

trait Doodad {
  type T
  val up: T => T
  val dr: T => TileRenderer => TileRenderer
  val light:T => Option[LightSource]
  val pos: T => Cell
  val self: T
  def update = Doodad(up, dr, light, pos, up(self))
  def getLight = light(self)
  def getPos = pos(self)
  def draw(tr:TileRenderer):TileRenderer = dr(self)(tr)
  def toUnloadable = Unloadable.fromPos[Doodad](this, _.getPos)
}
