package in.dogue.profundus.audio

import com.deweyvm.gleany.AssetLoader

object SoundManager {

  val dig = load("dig", 0.5)
  val swish = load("swish", 1.0)
  val land = load("land", 0.5)
  val dead = load("dead", 0.5)
  val item = load("item", 0.5)
  val shhh = load("shhh", 0.5)
  val boom = load("boom", 1.5)
  val step = load("step", 1.0)
  val `throw` = load("throw", 1.0)
  val stuck = load("stuck", 0.2)
  val blip = load("blip", 1.0)
  val clack = load("clack", 1.0)
  val blap = load("blap", 1.0)
  val wipe = load("wipe", 0.1)
  val pop = load("pop", 0.2)
  def load(s:String, adj:Double) = {
    val sound = AssetLoader.loadSound(s)
    sound.setAdjustVolume(adj.toFloat)
    sound
  }
}
