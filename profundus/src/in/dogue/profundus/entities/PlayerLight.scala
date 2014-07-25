package in.dogue.profundus.entities

import in.dogue.profundus.lighting.LightSource
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.audio.SoundManager

object PlayerLight {
  def create(small:LightSource, large:LightSource) = PlayerLight(small, large, 0, 0)

}

case class PlayerLight(small:LightSource, large:LightSource, t:Int, lt:Int) {
  final val flareTime = 60*60
  def update = {
    if (lt == 1) {
      SoundManager.fdown.playFull()
    }
    copy(t=t+1, lt=lt.drop1)
  }
  def useFlare = copy(lt=flareTime)
  def chooseLight = (lt > 0) select (small, large)
  def toLightSource(pos:Cell) = {
    chooseLight.copy(pos=pos, flicker=LightSource.flicker(t))
  }
}
