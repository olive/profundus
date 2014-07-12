package in.dogue.profundus.input

import com.deweyvm.gleany.input.triggers.{TriggerAggregate, KeyboardTrigger}
import com.deweyvm.gleany.input.{Control, AxisControl}
import com.badlogic.gdx.Input
import scala.collection.mutable.ArrayBuffer

object Controls {
  val All = ArrayBuffer[Control[Boolean]]()
  val Left = makeKb(Input.Keys.A)
  val Right = makeKb(Input.Keys.S)
  val Up = makeKb(Input.Keys.W)
  val Down = makeKb(Input.Keys.R)
  val Space = makeKb(Input.Keys.SPACE)
  val Action = makeKb(Input.Keys.N)
  val Capsule = makeKb(Input.Keys.E)
  val Escape = makeKb(Input.Keys.ESCAPE)

  val AxisX = new AxisControl(Left, Right)
  val AxisY = new AxisControl(Up, Down)

  def makeKb(key:Int) = {
    val result = new TriggerAggregate(Seq(new KeyboardTrigger(key)))
    All += result
    result
  }


  def update() {
    All foreach (_.update())
  }
}
