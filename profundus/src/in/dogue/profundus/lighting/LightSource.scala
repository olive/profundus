package in.dogue.profundus.lighting

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.geometry.Circle
import in.dogue.antiqua.Antiqua
import Antiqua._

object LightSource {
  def createCircle(pos:Cell, innerR:Int, outerR:Int) = {
    def light(c:Cell):Double = {
      val d = c.mag
       if (d < innerR) {
         1
       } else if (d > outerR) {
         0
       } else /* innerR < d < outerR*/{
         1 - (d - innerR) / (outerR - innerR)
       }

    }
    LightSource(pos, Circle.fill(0, 0, outerR, light))
  }
}

case class LightSource private (pos:Cell, private val cells:Seq[(Cell, Double)]) {
  def fill:Seq[(Cell, Double)] = {
    cells |+| pos
  }
}

