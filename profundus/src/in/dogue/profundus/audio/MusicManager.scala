package in.dogue.profundus.audio

import com.deweyvm.gleany.AssetLoader

object MusicManager {
  val chirps = load("night", 1.0)
  val noatm = load("noatm", 1.0)

  def load(name:String, d:Double) = {
    val m = AssetLoader.loadMusic(name)
    m.setAdjustVolume(d.toFloat)
    m
  }
}
case class MusicManager(j:Int) {
  println(j)
  def setPlayer(y:Int) = copy(j=y)
}
