package in.dogue.profundus.entities

import in.dogue.profundus.audio.SoundManager

class StepMachine {
  var i = 0
  def increment() = {
    i += 1
    println(i)
    if (i %6 == 0) {
      SoundManager.step.play()
    }
  }

}
