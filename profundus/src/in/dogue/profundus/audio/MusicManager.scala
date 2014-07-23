package in.dogue.profundus.audio

import com.deweyvm.gleany.AssetLoader
import com.deweyvm.gleany.audio.Music
import in.dogue.profundus.entities.{Alive, LivingState}
import in.dogue.antiqua.Antiqua
import Antiqua._

object MusicManager {
  val chirps = load("night", 1.0)
  val notatm = load("notatm", 0.2)

  def load(name:String, d:Double) = {
    val m = AssetLoader.loadMusic(name)
    m.setAdjustVolume(d.toFloat)
    m
  }
}
case class MusicManager(j:Int, pState:LivingState, worldRows:Int) {
  val cricketVol = if (j < worldRows/2) {
    1
  } else {
    1 - (j.toDouble - worldRows/2)/(worldRows*2 - worldRows/2)
  }

  val caveVol = if (j > worldRows) {
    (j.toDouble - worldRows)/(worldRows)
  } else {
    0
  }
  if (pState == Alive) {
    //println("setting fade")
    setFade(MusicManager.chirps, cricketVol, MusicManager.notatm, caveVol)
  } else {
    halt()
  }


  def setPlayer(y:Int, state:LivingState) = copy(j=y, pState=state)
  private def check(m:Music, d:Double) = {
    if (d > 0 && !m.isPlaying) {
      m.resume()
    } else if (d <= 0 && m.isPlaying) {
      m.pause()
    }
  }
  def setFade(m1:Music, d1:Double, m2:Music, d2:Double) {
    m1.setVolume(d1.toFloat.clamp(0,1))
    m2.setVolume(d2.toFloat.clamp(0,1))

    check(m1, d1)
    check(m2, d2)
  }

  def halt() {
    check(MusicManager.chirps, 0)
    check(MusicManager.notatm, 0)
  }
}
