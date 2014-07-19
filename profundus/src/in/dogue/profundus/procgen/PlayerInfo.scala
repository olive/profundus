package in.dogue.profundus.procgen

import com.deweyvm.gleany.{Glean, AssetLoader}
import scala.util.Random


object PlayerInfo {

  private def loadLines(name:String) = {
    Glean.y.files.data("namegen/%s".format(name)).readString().split("\n").toVector
  }


  val adj = loadLines("adjectives")
  val titleNouns = loadLines("titlenouns")
  val gerunds = loadLines("weaknessgerunds")
  val articles = Vector("a", "the")
  val jobGerunds = loadLines("jobgerunds")
  val jobNouns = loadLines("jobnouns")
  val weaknessNouns = loadLines("weaknessnouns")

}
class PlayerInfo(r:Random) {
  import PlayerInfo._
  val name = Syllabary.mkWord(2)(r).cap
  val title = new Title(adj, titleNouns, r).string(name)
  val weakness = new Weakness(gerunds, articles, weaknessNouns, adj, r).string
  val job = new Job(jobNouns, jobGerunds, r).string
  val craving = new Craving(adj, jobNouns, r).string
  //name, name the adjective, adjective name
  //previous occupation, noun verber
  //weakness verbing noun, adjective verbing
}
