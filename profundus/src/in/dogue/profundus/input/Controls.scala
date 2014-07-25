package in.dogue.profundus.input

import com.deweyvm.gleany.input.triggers.{JoypadTrigger, TriggerAggregate, KeyboardTrigger}
import com.deweyvm.gleany.input.{JoypadButton, FaceButton, Control, AxisControl}
import com.badlogic.gdx.Input
import scala.collection.mutable.ArrayBuffer

object Controls {
  val All = ArrayBuffer[Control[Boolean]]()
  val Left = makePr(Input.Keys.LEFT, JoypadButton("DPadLeft"))
  val Right = makePr(Input.Keys.RIGHT, JoypadButton("DPadRight"))
  val Up = makePr(Input.Keys.UP, JoypadButton("DPadUp"))
  val Down = makePr(Input.Keys.DOWN, JoypadButton("DPadDown"))
  val Space = makePr(Input.Keys.SPACE, JoypadButton("2"))
  val Action = makePr(Input.Keys.C, JoypadButton("3"))
  val Capsule = makePr(Input.Keys.X, JoypadButton("6"))
  val Kill = makePr(Input.Keys.K, JoypadButton("7"))
  val Rope = makePr(Input.Keys.Z, JoypadButton("8"))
  val Drop = makePr(Input.Keys.V, JoypadButton("9"))
  val Pause = makePr(Input.Keys.P, JoypadButton("10"))
  val Help = makePr(Input.Keys.H, JoypadButton("5"))
  val Kick = makePr(Input.Keys.SHIFT_LEFT, JoypadButton("4"))
  val Debug = makeKb(Input.Keys.TAB)
  val Escape = makeKb(Input.Keys.ESCAPE)

  val AxisX = new AxisControl(Left, Right)
  val AxisY = new AxisControl(Up, Down)

  def makeKb(key:Int) = {
    val result = new TriggerAggregate(Seq(new KeyboardTrigger(key)))
    All += result
    result
  }

  def makePr(key:Int, ctrl:JoypadButton): TriggerAggregate = {
    val result = new TriggerAggregate(Seq(new KeyboardTrigger(key), new JoypadTrigger(ctrl)))
    All += result
    result
  }


  def update() {
    All foreach (_.update())
  }
}
