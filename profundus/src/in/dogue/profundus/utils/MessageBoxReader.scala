package in.dogue.profundus.utils

import com.deweyvm.gleany.Glean

object MessageBoxReader {
  def load(name:String) = {
    val all = Glean.y.files.data(name).readString()
    val boxes = all.split("@")
    boxes.toVector
  }
}
