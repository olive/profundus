package in.dogue.profundus.procgen

import scala.util.Random
import in.dogue.antiqua.Antiqua
import Antiqua._

class Job(nouns:Vector[String], gerunds:Vector[String], r:Random) {
  val noun = nouns.randomR(r)
  val gerund = gerunds.randomR(r)
  def string = "%s %s".format (noun, gerund)
}
