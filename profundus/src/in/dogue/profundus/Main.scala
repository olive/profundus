package in.dogue.profundus

import com.deweyvm.gleany.{GleanyConfig, GleanyInitializer, GleanyGame}
import com.deweyvm.gleany.data.Point2i
import com.deweyvm.gleany.saving.{Settings, SettingDefaults}
import com.deweyvm.gleany.files.PathResolver
import com.deweyvm.gleany.logging.Logger
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.world.dungeon.Dungeon
import java.util.Random

object Main {
  def main(args: Array[String]) {
    Dungeon.create(15, 15, 0.5, new Random()).saveImage(new Random())
    System.exit(1)
    val iconPath = "sprites/icon.gif"
    val settings = new Settings(ProfundusControls, new SettingDefaults() {
      val SfxVolume: Float = 0.2f
      val MusicVolume: Float = 0.2f
      val WindowSize: Point2i = Point2i(512,512 + 256)
      val DisplayMode: Int = 0
    }, false)
    val config = new GleanyConfig(settings, "profundus", iconPath.some)
    val pathResolver = new PathResolver(
      "fonts",
      "sprites",
      "sfx",
      "music",
      "data",
      "shaders",
      "maps"
    )
    Logger.attachCrasher(".")
    val initializer = new GleanyInitializer(pathResolver, settings)
    GleanyGame.runGame(config, new Game(initializer))

  }
}
