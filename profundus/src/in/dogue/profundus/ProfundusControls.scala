package in.dogue.profundus

import com.deweyvm.gleany.saving.{ControlName, ControlNameCollection}


class ProfundusControl(descriptor: String) extends ControlName {
  override def name: String = descriptor
}

object ProfundusControls extends ControlNameCollection[ProfundusControl] {
  def fromString(string: String): Option[ProfundusControl] = None
  def makeJoypadDefault: Map[String,String] = Map()
  def makeKeyboardDefault: Map[String,java.lang.Float] = Map()
  def values: Seq[ProfundusControl] = Seq()
}
