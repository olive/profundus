package in.dogue.profundus.utils

import com.deweyvm.gleany.Glean

object MessageBoxReader {
  def load(name:String) = {
    val all = Glean.y.files.data(name).readString()
    val boxes = all.split("@").map { s => strip1(s, '\n') }
    boxes.toVector
  }

  private def strip1(s:String, char:Char): String = {
    s match {
      case a if a.length > 0 && a(0) == char => a.drop(1)
      case a => a
    }
  }
}
