package in.dogue.profundus.world

import com.deweyvm.gleany.graphics.Color

class StratumColor(hue:Double) {
  val color = Color.fromHsb(hue)
  def ways1[T](vec:Vector[T]) = {
    val count = vec.length
    val index = (hue*count).toInt
    vec(index)
  }
  def ways2[T](vec:Vector[T]) = {
    val count = vec.length
    val index = (hue*count).toInt
    (vec(index), vec((index+1) % vec.length))
  }


  def ways3[T](vec:Vector[T]) = {
    val count = vec.length
    val index = (hue*count).toInt
    (vec(index), vec((index+1) % vec.length), vec((index+2) % vec.length))
  }
}
