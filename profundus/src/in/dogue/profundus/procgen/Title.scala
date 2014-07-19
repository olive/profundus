package in.dogue.profundus.procgen

import scala.util.Random
import in.dogue.antiqua.Antiqua
import Antiqua._

object TitleType {
  def randomR(r:Random) = {
    if (r.nextBoolean) {
      AdjectiveNoun.apply _
    } else {
      NounArticleAdjective.apply _
    }
  }
}
sealed trait TitleType {
  def string(name:String):String
}
case class AdjectiveNoun(adj:String, noun:String) extends TitleType {
  override def string(name:String) = adj.capitalize + " " + noun.capitalize + " " + name
}
case class NounArticleAdjective(adj:String, noun:String) extends TitleType {
  override def string(name:String) = name + " the " + adj.capitalize + " " + noun.capitalize
}

class Title(adjs:Vector[String], nouns:Vector[String], r:Random) {
  val typ =  TitleType.randomR(r)
  val adj = adjs.randomR(r)
  val noun = nouns.randomR(r)
  def string(name:String) = typ(adj, noun).string(name)
}
