package in.dogue.profundus.procgen

import scala.util.Random
import in.dogue.antiqua.Antiqua
import Antiqua._

class Craving(adjs:Vector[String], nouns:Vector[String], r:Random) {
  def string = adjs.randomR(r) + " " + nouns.randomR(r)
}
