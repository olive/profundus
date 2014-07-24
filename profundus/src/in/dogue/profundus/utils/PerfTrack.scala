package in.dogue.profundus.utils

import scala.collection.mutable.ArrayBuffer
import in.dogue.profundus.Game

class PerfTrack(trackerName:String) {
  private val freq = 30
  private var lastTracked = 0
  private val map = collection.mutable.Map[String,Double]().withDefaultValue(0)
  private val tick = collection.mutable.Map[String,Int]().withDefaultValue(0)
  private val mmap = collection.mutable.Map[String,Double]().withDefaultValue(0)

  def track[T](name:String)(f: => T) = {
    lastTracked = Game.t
    val time = System.nanoTime
    val res = f
    val long = System.nanoTime - time
    map(name) += long/1000000.0
    tick(name) += 1
    if (tick(name) % freq == 0) {
      tick(name) = 0
      mmap(name) = map(name)/freq.toDouble
      map(name) = 0
    }
    res
  }
  val s = "Total"
  def getLongest:Int = {
    val longest = (mmap.keys.toSeq :+ s).maxBy{_.length}.length

    longest
  }

  def print(longest:Int):Seq[String] = {

    val total = mmap.values.sum


    val result = ArrayBuffer[String]()
    result += trackerName
    if (Game.t - lastTracked > freq) {
      result += "stale"
      return result
    }
    for ((name,amt) <- mmap) {
      result += (" " * (longest - name.length) + "%s: %02d%%".format(name, ((amt/total) * 100).toInt))
    }

    result += " " * (longest - s.length) + "Total: %.2fms".format(total)
    result
  }



}
