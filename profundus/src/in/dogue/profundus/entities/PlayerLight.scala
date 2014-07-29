package in.dogue.profundus.entities

import in.dogue.profundus.lighting.LightSource
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.audio.SoundManager

object PlayerLight {
  def create(f:Int => LightSource) = {
    PlayerLight(f, 0, 0)
  }

}

case class PlayerLight(f:Int => LightSource, t:Int, lt:Int) {
  final val flareTime = 60*60
  def update = {
    if (lt == 1) {
      SoundManager.fdown.playFull()
    }
    copy(t=t+1, lt=lt.drop1)
  }
  def useFlare = copy(lt=flareTime)
  def chooseLight(attr:Attributes) = {
    val outer = attr.lightRadius.toInt + (lt > 0).select (0, attr.flareMod)
    val inner = (outer - 5).clamp(0, Int.MaxValue)
    LightSource.createCircle((0,0), inner, outer, 1)
  }
  def toLightSource(attr:Attributes, pos:Cell) = {
    chooseLight(attr).copy(pos=pos, flicker=LightSource.flicker(t))
  }
}
