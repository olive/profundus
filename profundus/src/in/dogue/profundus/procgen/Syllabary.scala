package in.dogue.profundus.procgen

import in.dogue.antiqua.Antiqua
import Antiqua._
import scala.util.Random

object Syllabary {
  class VowelHelper(s:(String, Double)) {
    def v = Vowel(s._1, s._2)
  }
  implicit def string2VowelHelper(s:(String, Double)) = new VowelHelper(s)
  class ConsonantHelper(s:(String, Double)) {
    def c = Consonant(s._1, s._2)
  }
  implicit def string2ConsonantHelper(s:(String, Double)) = new ConsonantHelper(s)
  //amt value based on consonants, allowable distance based on vowel
  val vowels = Vector(
    ("o", 0.5), //g(oa)t
    ("u", 0.5), //b(oo)t
    ("i", 0.5), //f(i)t
    ("a", 0.5), //b(o)ther
    ("ì", 0.5)  //f(ee)t
  ).map{_.v}

  val consonants = Vector(
    ("gn", 1.0), //(gn)osis
    ("th", 1.0),
    ( "h", 1.0),
    ("kn", 0.8), //(kn)ytt
    ( "p", 0.6),
    ( "t", 0.5),
    ( "d", 0.5),
    ( "dz", 0.25),  //wa(des)
    ( "j", 0.2), //(y)ell
    ( "w", 0.2),
    ( "z", 0.0), //(z)oo
    ( "s", 0.0)  //(s)ue
  ).map {_.c}
  val All = {
    for (v <- vowels ; c <- consonants) yield {
      Syllable(c, v)
    }
  }
  def mkWord(len:Int)(r:Random) = {
    val first = All.randomR(r)
    val second = All.filter{ syl =>
      math.abs(syl.c.amt - first.c.amt) <= first.v.amt
    }.randomR(r)
    Word(Seq(first, second))
  }

}

case class Consonant(s:String, amt:Double)
case class Vowel(s:String, amt:Double)
case class Syllable(c:Consonant, v:Vowel) {
  def string = c.s + v.s
}
case class Word(syls:Seq[Syllable]) {
  def string = syls.map{_.string}.mkString
  def cap = string.capitalize
}
