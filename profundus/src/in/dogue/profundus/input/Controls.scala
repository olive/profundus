package in.dogue.profundus.input

import com.deweyvm.gleany.input.triggers.{TriggerAggregate, KeyboardTrigger}
import com.deweyvm.gleany.input.{Control, AxisControl}
import com.badlogic.gdx.Input
import scala.collection.mutable.ArrayBuffer

object Controls {
  val All = ArrayBuffer[Control[Boolean]]()
  val Left = makeKb(Input.Keys.LEFT)
  val Right = makeKb(Input.Keys.RIGHT)
  val Up = makeKb(Input.Keys.UP)
  val Down = makeKb(Input.Keys.DOWN)
  val Space = makeKb(Input.Keys.SPACE)
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
