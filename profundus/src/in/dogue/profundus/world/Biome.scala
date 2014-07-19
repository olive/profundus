package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.procgen.PerlinNoise
import in.dogue.profundus.Profundus
import in.dogue.antiqua.data.Direction
import in.dogue.profundus.doodads.Doodad
import com.deweyvm.gleany.data.Recti

object Biome {
  def createDummy = {
    val ts = TerrainScheme.dummy
    val tg = TerrainGenerator.dummy(ts)
    val fg = FeatureGenerator.dummy
    val eg = EntityGenerator.dummy
    val dg = DoodadGenerator.dummy
    Biome(ts, tg, fg, eg, dg)
  }
}

case class Biome(ts:TerrainScheme, tg:TerrainGenerator, fg:FeatureGenerator, eg:EntityGenerator, dg:DoodadGenerator) {
  def generate(cols:Int, rows:Int, y:Int, r:Random):(Biome, Terrain, Seq[WorldSpawn]) = {
    import Profundus._
    val (spawn, face, features) = if (y < 0) {
      ((0,0), Direction.Down, Seq(Terrain.skyFeature(cols, rows)))
    } else if (y == 0) {
      val (spawn, face, lines, circle) = Terrain.makeLines(cols, rows, r)//createMouth
      val f = Feature(Recti(0,0,cols,rows),Terrain.createMouth(lines, circle))
      (spawn, face, Seq(f))
    } else {
      ((0,0), Direction.Down, fg.f(cols, rows, y, ts, r))
    }

    val noise = new PerlinNoise().generate(cols, rows, 0, y, r.nextInt())
    val tiles = noise.map { case (i, j, d) =>
      val state = tg.mkTile(ts, i, j, y, cols, rows, d, r)
      WorldTile(state(r))
    }
    val seed = (tiles, Seq[Doodad[_]]())
    val (newTiles, ds) = features.foldLeft(seed) { case ((innerTiles, doods), ft) =>
      ft.transform(cols, rows, y*rows, ts, innerTiles, r)

    }
    val doodads = dg.generate(ts, newTiles, r) ++ ds
    val (creatures, casques) = eg.generate(cols, rows, y, newTiles, r)

    (this, Terrain(y*rows, newTiles, doodads, spawn, face), Seq(creatures.ws, casques.ws))
  }
}
