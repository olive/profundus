package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.procgen.PerlinNoise
import in.dogue.profundus.Profundus
import in.dogue.antiqua.data.{Array2d, Direction}
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.Profundus._
import in.dogue.profundus.world.features.Lair

object Stratum {
  def createDummy = {
    val ts = TerrainScheme.generate(new Random())
    val tg = TerrainGenerator.dummy(ts)
    val fg = FeatureGenerator.dummy
    val eg = EntityGenerator.dummy
    val dg = DoodadGenerator.dummy
    val pg = PickupGenerator.dummy
    Stratum(ts, tg, fg, eg, dg, pg)
  }

  def createSurface = {

  }
}

case class Stratum(ts:TerrainScheme, tg:TerrainGenerator, fg:FeatureGenerator, eg:EntityGenerator, dg:DoodadGenerator, pg:PickupGenerator) {
  val strataSize = 4


  def generateAboveGround(cols:Int, rows:Int, yIndex:Int, r:Random):(Stratum, Terrain, Seq[WorldSpawn], Seq[GlobalSpawn]) = {
    val (spawn, face, features) = if (yIndex < 0) {
      ((0,0), Direction.Down, Seq(Terrain.skyFeature(cols, rows)))
    } else /*(yIndex == 0)*/ {
      val (spawn, face, (lines, circle)) = Terrain.makeLines(cols, rows, r) //createMouth
      val f = Feature(Recti(0, 0, cols, rows), Terrain.createMouth(face, lines, circle))
      (spawn, face, Seq(f))
    }
    val noise = new PerlinNoise().generate(cols, rows, 0, yIndex, r.nextInt())
    val tiles = noise.map { case (ij, d) =>
      val state = tg.mkTile(ts, ij, yIndex, cols, rows, d, r)
      WorldTile(state(r))
    }


    val (newTiles, ds, gs) = fold3(tiles, features) { case (ft, tiles) =>
      ft.transform(cols, rows, yIndex * rows, ts, tiles, r)
    }

    val pickups = Seq()
    val doodads = dg.generate(ts, newTiles, r) ++ ds
    val entities = eg.generate(cols, rows, yIndex, ts, newTiles, r)
    val newBiome = this
    (newBiome, Terrain(yIndex*rows, newTiles, doodads, spawn, face), Seq(entities) ++ pickups, gs)
  }

  def generateLair(cols:Int, rows:Int, yIndex:Int, r:Random):(Stratum, Terrain, Seq[WorldSpawn], Seq[GlobalSpawn]) = {
    val spawn = (0,0)
    val face = Direction.Down
    val features = Seq(new Lair().toFeature(cols, rows))
    val noise = new PerlinNoise().generate(cols, rows, 0, yIndex, r.nextInt())
    val tiles = noise.map { case (ij, d) =>
      val state = tg.mkTile(ts, ij, yIndex, cols, rows, d, r)
      WorldTile(state(r))
    }


    val (newTiles, ds, gs) = fold3(tiles, features) { case (ft, tiles) =>
      ft.transform(cols, rows, yIndex * rows, ts, tiles, r)
    }


    val pickups = pg.generate(cols, rows, yIndex*rows, newTiles, ts, r)

    val doodads = dg.generate(ts, newTiles, r) ++ ds
    val entities = eg.generate(cols, rows, yIndex, ts, newTiles, r)
    val newBiome = if (yIndex % strataSize == 0) {
      copy(ts=TerrainScheme.generate(r))
    } else {
      this
    }
    (newBiome, Terrain(yIndex*rows, newTiles, doodads, spawn, face), Seq(entities) ++ pickups, gs)
  }

  def generateOld(cols:Int, rows:Int, yIndex:Int, r:Random):(Stratum, Terrain, Seq[WorldSpawn], Seq[GlobalSpawn]) = {
    import Profundus._

    if (yIndex <= 0) {
      return generateAboveGround(cols, rows, yIndex, r)
    } else if (yIndex > 5) {
      return generateLair(cols, rows, yIndex, r)
    }
    val (spawn, face, features) = ((0,0), Direction.Down, fg.assemble(cols, rows, yIndex, ts, r))
    val noise = new PerlinNoise().generate(cols, rows, 0, yIndex, r.nextInt())
    val tiles = noise.map { case (ij, d) =>
      val state = tg.mkTile(ts, ij, yIndex, cols, rows, d, r)
      WorldTile(state(r))
    }


    val (newTiles, ds, gs) = fold3(tiles, features) { case (ft, tiles) =>
      ft.transform(cols, rows, yIndex * rows, ts, tiles, r)
    }


    val pickups = pg.generate(cols, rows, yIndex*rows, newTiles, ts, r)

    val doodads = dg.generate(ts, newTiles, r) ++ ds
    val entities = eg.generate(cols, rows, yIndex, ts, newTiles, r)
    val newBiome = if (yIndex % strataSize == 0) {
      copy(ts=TerrainScheme.generate(r))
    } else {
      this
    }
    (newBiome, Terrain(yIndex*rows, newTiles, doodads, spawn, face), Seq(entities) ++ pickups, gs)
  }

  def generate(cols:Int, rows:Int, yIndex:Int, r:Random):(Stratum, Terrain, Seq[WorldSpawn], Seq[GlobalSpawn]) = {
    import Profundus._


    val (spawn, face, features) = ((0,0), Direction.Down, fg.assemble(cols, rows, yIndex, ts, r))
    val noise = new PerlinNoise().generate(cols, rows, 0, yIndex, r.nextInt())
    val tiles = noise.map { case (ij, d) =>
      val state = tg.mkTile(ts, ij, yIndex, cols, rows, d, r)
      WorldTile(state(r))
    }


    val (newTiles, ds, gs) = fold3(tiles, features) { case (ft, tiles) =>
      ft.transform(cols, rows, yIndex * rows, ts, tiles, r)
    }


    val pickups = pg.generate(cols, rows, yIndex*rows, newTiles, ts, r)

    val doodads = dg.generate(ts, newTiles, r) ++ ds
    val entities = eg.generate(cols, rows, yIndex, ts, newTiles, r)
    val newBiome = if (yIndex % strataSize == 0) {
      copy(ts=TerrainScheme.generate(r))
    } else {
      this
    }
    (newBiome, Terrain(yIndex*rows, newTiles, doodads, spawn, face), Seq(entities) ++ pickups, gs)
  }

}
