package in.dogue.profundus

import com.deweyvm.gleany.{Glean, GleanyGame, GleanyInitializer}
import java.util.concurrent.{Executors, Callable, TimeUnit}
import in.dogue.profundus.input.Controls
import scala.util.Random
import in.dogue.antiqua.Antiqua
import Antiqua._

object Game {
  var t = 0
  val debug = true
  val fixedSeed = true && debug
  val flyMode = false && debug
  val invMode = false && debug
  val lightsOn = false && debug
  val version = "Version 0.0.11"
  def getSeed = (fixedSeed).select(new Random().nextInt(), 2)
}
class Game(initializer: GleanyInitializer) extends GleanyGame(initializer) {
  private lazy val engine = new Engine()
  override def update() {
    Controls.update()
    engine.update()
    Game.t += 1
  }

  override def draw() {
    engine.draw()
  }

  override def resize(width: Int, height: Int) {
    Glean.y.settings.setWindowSize(width, height)
  }

  override def dispose() {
    val executor = Executors.newSingleThreadExecutor()
    executor.invokeAll(java.util.Arrays.asList(new Callable[Unit] {
      override def call(): Unit = ()
    }), 2, TimeUnit.SECONDS)
    executor.shutdown()
  }
}
