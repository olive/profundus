package in.dogue.profundus.lighting

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.geometry.Circle
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.data.Array2d

object LightSource {
  def createCircle(pos:Cell, innerR:Int, outerR:Int, dim:Double): LightSource = {
    def light(c:Cell):Double = {
      val d = c.mag
      val intensity = if (d < innerR) {
          1
        } else if (d > outerR) {
          0
        } else /* innerR < d < outerR*/{
          1 - (d - innerR) / (outerR - innerR)
        }
      intensity*dim

    }
    def onScreen(cxy:(Int,Int), recti:Recti) = {
      Circle(pos |+| cxy, outerR).intersects(recti)
    }
    LightSource(pos, 1, Circle.fill(0, 0, outerR, light), onScreen)
  }

  def createRect(pos:Cell/*upper left!*/, width:Int, height:Int, dim:Double): LightSource = {
    val cells = Array2d.tabulate(width, height) { case (i, j) =>
      dim*(1 - j/height.toDouble)
    }.flatten.map { case (i, j, d) => ((i, j), d)}
    def onScreen(cxy:(Int,Int), recti:Recti) = {
      (Recti(pos.x, pos.y, width, height) + Recti(cxy.x, cxy.y, 0, 0)).intersects(recti)
    }
    LightSource(pos, 1, cells, onScreen)
  }

  def createAnnulus(center:Cell, innerR:Int, outerR:Int, dim:Double): LightSource = {
    createCircle(center, innerR, outerR, dim)
  }

  def flicker(t:Int) = {
    import scala.math._
    (abs(sin(sin(sin(sin(t/5.0) + t/50.0) + t/500.0) + t/5000.0)) + 5)/6
  }
}

case class LightSource private (pos:Cell, flicker:Double, private val cells:Seq[(Cell, Double)], onScreen: ((Int,Int), Recti) => Boolean) {
  def fill:Seq[(Cell, Double)] = {
    (cells |+| pos).smap { _ * flicker}
  }
}

