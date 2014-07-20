package in.dogue.profundus.ui

import in.dogue.antiqua.ui.{TextLine, TextBox}
import in.dogue.profundus.input.Controls
import in.dogue.antiqua.graphics.{Text, TextFactory, TileRenderer}
import in.dogue.antiqua.data.CP437
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.audio.SoundManager

object MessageBox {
  def create[T](tf:TextFactory, boxes:Seq[String], onFinish: () => T) = {
    val seq = boxes.map { s =>
      val lines = tf.textLines(s)
      def play() {
        SoundManager.blip.stop()
        SoundManager.blip.play()
      }
      def mkLine(l:Text) = TextLine.create(l, play, c => c != CP437.` `.toCode)
      val textLines = lines.map(mkLine)
      TextBox.create(textLines.toVector)
    }
    MessageBox(seq.toVector, onFinish, 0)
  }
}

sealed trait MessageBoxResult[T]
case class MessageBoxContinue[T](mb:MessageBox[T]) extends MessageBoxResult[T]
case class MessageBoxComplete[T](t:T) extends MessageBoxResult[T]

case class MessageBox[T](boxes:IndexedSeq[TextBox], onFinish:() => T, ptr:Int) {
  def pagePending = ptr < boxes.length - 1 && boxes(ptr).isFinished
  def isFinished = ptr == boxes.length - 1 && boxes(ptr).isFinished
  def height = boxes(ptr).lines.length
  def update = {
    if (isFinished && Controls.Space.justPressed) {
      MessageBoxComplete(onFinish())
    } else {
      MessageBoxContinue(updateSelf)
    }
  }

  private def updateSelf = {
    if (boxes(ptr).isFinished && ptr < boxes.length - 1 && Controls.Space.justPressed) {
      copy(ptr=ptr+1)
    } else {
      val newBoxes = boxes.updated(ptr, boxes(ptr).update)
      copy(boxes=newBoxes)
    }
  }

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< boxes(ptr).draw(i, j)
  }
}
