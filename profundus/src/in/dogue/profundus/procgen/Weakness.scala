package in.dogue.profundus.procgen

import scala.util.Random
import in.dogue.antiqua.Antiqua
import Antiqua._


object WeaknessType {
  def randomR(r:Random) = {
    val d = r.nextDouble
    if (d < 0.33) {
      GerundArticleNoun.create _
    } else if (d < 0.66) {
      AdjectiveNounWeak.create _
    } else {
      NounGerund.create _
    }
  }
}

case class WeaknessArgs(ger:Vector[String], art:Vector[String], nouns:Vector[String], adj:Vector[String])

sealed trait WeaknessType {
  def string:String
}

object GerundArticleNoun {
  def create(args:WeaknessArgs, r:Random) = {
    GerundArticleNoun(args.ger.randomR(r), args.art.randomR(r), args.nouns.randomR(r))
  }
}
case class GerundArticleNoun(ger:String, art:String, noun:String) extends WeaknessType {
  override def string = {
    val nf = noun(0)
    val aart = if (art == "a" && "aeiou".contains(nf)) {
      "an"
    } else {
      art
    }
    "%s %s %s" format(ger, aart, noun)
  }
}


object AdjectiveNounWeak {
  def create(args:WeaknessArgs, r:Random) = {
    AdjectiveNounWeak(args.adj.randomR(r), args.nouns.randomR(r))
  }
}
case class AdjectiveNounWeak(adj:String, noun:String) extends WeaknessType {
  override def string = "%s %s" format (adj, noun)
}

object NounGerund {
  def create(args:WeaknessArgs, r:Random) = {
    NounGerund(args.nouns.randomR(r), args.ger.randomR(r))
  }
}
case class NounGerund(noun:String, ger:String) extends WeaknessType {
  override def string = "%s %s" format (noun, ger)
}

class Weakness(gerunds:Vector[String], articles:Vector[String], nouns:Vector[String], adj:Vector[String], r:Random) {
  val args = WeaknessArgs(gerunds, articles, nouns, adj)
  val typ = WeaknessType.randomR(r)
  def string = typ(args, r).string
}
