package in.dogue.profundus.entities

import in.dogue.profundus.lighting.LightSource
import in.dogue.antiqua.Antiqua.Cell

object PlayerLight {
  def create(ls:LightSource) = PlayerLight(ls, 0)
}

case class PlayerLight(ls:LightSource, t:Int) {
  def update = copy(t=t+1)
  private def getFlicker = {
    import scala.math._
    (abs(sin(sin(sin(sin(t/5.0) + t/50.0) + t/500.0) + t/5000.0)) + 5)/6
  }
  def get(pos:Cell) = {
    println("Pos" + pos)
    ls.copy(pos=pos, flicker=getFlicker)
  }
}
