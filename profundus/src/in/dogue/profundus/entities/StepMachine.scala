package in.dogue.profundus.entities

import in.dogue.profundus.audio.SoundManager
import in.dogue.profundus.Game

class StepMachine {
  var i = 0
  def increment() = {
    i += 1
    if (i % 7 == 0) {
      SoundManager.step.play()
      i += 1
    }
  }

}
