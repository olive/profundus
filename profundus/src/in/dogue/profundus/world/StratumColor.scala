package in.dogue.profundus.world

class StratumColor(hue:Double) {
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
