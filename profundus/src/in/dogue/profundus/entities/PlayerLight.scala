package in.dogue.profundus.entities

import in.dogue.profundus.lighting.LightSource
import in.dogue.antiqua.Antiqua.Cell

object PlayerLight {
  def create(ls:LightSource) = PlayerLight(ls, 0)

}

case class PlayerLight(ls:LightSource, t:Int) {
  def update = copy(t=t+1)

  def toLightSource(pos:Cell) = {
    ls.copy(pos=pos, flicker=LightSource.flicker(t))
  }
}
