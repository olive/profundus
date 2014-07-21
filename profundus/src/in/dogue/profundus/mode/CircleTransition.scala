package in.dogue.profundus.mode

import in.dogue.antiqua.graphics.{Text, Tile, TileRenderer}
import in.dogue.antiqua.Antiqua
import Antiqua._
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.{FutureError, FutureFinished, FutureComputing, Future}
import in.dogue.profundus.Profundus
import in.dogue.antiqua.utils.FormatExc
import in.dogue.profundus.audio.SoundManager

object CircleTransition {
  def create(cols:Int, rows:Int, old:Mode[_], `new`:() => Mode[_], seed:Option[Int]) = {
    val tf = Profundus.tf
    CircleTransition(cols, rows, old, new Future(`new`), tf.create("LOADING"), 30, CircleIn(0), seed)
  }
}



sealed trait CircleState {
  def getT:Int
}
case class CircleIn(t:Int) extends CircleState { override def getT = t }
case class WaitLoad(t:Int, wlt:Int) extends CircleState { override def getT = t }
case class CircleOut(t:Int) extends CircleState { override def getT = t }
case class CircleDone(max:Int) extends CircleState { override def getT = max }
case class CircleFail(msg:TileGroup) extends CircleState { override def getT = 0 }
case class CircleTransition private (cols:Int, rows:Int, old:Mode[_], `new`:Future[Mode[_]], text:Text, max:Int, state:CircleState, seed:Option[Int]) {
  def update = {
    val newState: CircleState = state match {
      case CircleIn(t) =>
        if (t == 0) {
          SoundManager.wipe.play()
        }
        if (t > max/2) {
          WaitLoad(t+1, 0)
        } else {
          CircleIn(t + 1)
        }
      case WaitLoad(t, wlt) =>
        `new`.update match {
          case FutureFinished(_) => CircleOut(t)
          case FutureComputing => WaitLoad(t, wlt+1)
          case FutureError(e) => {
            val fmt = FormatExc.smallFormat(cols, e)
            val pre = "Oops, an error occurred.\n\nSeed: %d.\n\n".format(seed.getOrElse(-1))
            val post = "\n\nA detailed log of this error\nhas been saved.\n\nPlease restart profundus."
            CircleFail(Profundus.tf.multiline(pre + fmt + post))
          }
        }
      case CircleOut(t) => if (t > max) {
        CircleDone(max)
      } else {
        CircleOut(t+1)
      }
      case c@CircleDone(_) => c
      case c@CircleFail(_) => c
    }
    val newThis = copy(state=newState).toMode
    newState match {
      case CircleDone(_) =>
        `new`.update match {
          case FutureFinished(m) => m
          case _ => newThis
        }
      case a => newThis
    }
  }

  private def drawCover(tr:TileRenderer):TileRenderer = {
    val half = max/2
    val col2 = cols/2.sqrt
    def f(tile:Tile) = tile.setBg(Color.Black).setFg(Color.Black)
    val t = state.getT
    val tt = if (t < half) {
      half - t
    } else {
      t - half
    }
    val r = ((tt/half.toFloat)*col2).toInt


    val draws = for (i <- 0 until cols; j <- 0 until rows) yield {
      val hyp = scala.math.hypot(i - cols/2, j - rows/2)
      ((i, j), f _).onlyIf(hyp >= r)

    }
    tr `$$>` draws.flatten

  }

  def draw(tr:TileRenderer):TileRenderer = {
    def drawNew = `new`.update match {
      case FutureFinished(m) => m.draw _
      case FutureComputing => id[TileRenderer] _
      case FutureError(msg) => id[TileRenderer] _
    }
    tr <+< (state match {
      case CircleIn(_) => old.draw
      case WaitLoad(_,_) => id[TileRenderer]
      case CircleOut(_) => drawNew
      case CircleDone(_) => drawNew
      case CircleFail(_) => id[TileRenderer]
    }) <+< drawCover <+< (state match {
      case WaitLoad(_,wlt) if wlt > 3 => text.draw((10,10))
      case CircleFail(tg) => (tr:TileRenderer) => tr <++ (tg |++| ((1,1)))
      case a => id[TileRenderer]
    })
  }

  def toMode:Mode[CircleTransition] = Mode(_.update, _.draw, this)
}
