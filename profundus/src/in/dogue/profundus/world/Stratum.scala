package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.procgen.PerlinNoise
import in.dogue.profundus.Profundus

object Stratum {
  def createDummy(r:Random) = {
    val ts = TerrainScheme.generate(r)
    val tg = TerrainGenerator.dummy(ts)
    val fg = FeatureGenerator.dummy
    val eg = EntityGenerator.dummy
    val dg = DoodadGenerator.empty
    val pg = PickupGenerator.dummy
    val sg = SpawnGenerator.dummy
    Stratum(ts, tg, fg, eg, dg, pg, sg)
  }

  def createSurface(r:Random) = {
    val ts = TerrainScheme.generate(r)
    val sg = SpawnGenerator.surface
    val tg = TerrainGenerator.dummy(ts)
    val fg = FeatureGenerator.surface
    val eg = EntityGenerator.empty
    val dg = DoodadGenerator.empty
    val pg = PickupGenerator.empty
    Stratum(ts, tg, fg, eg, dg, pg, sg)
  }

  def createLair(r:Random):Stratum = {
    val ts = TerrainScheme.generate(r)
    val sg = SpawnGenerator.dummy
    val tg = TerrainGenerator.dummy(ts)
    val fg = FeatureGenerator.lair
    val eg = EntityGenerator.lair
    val dg = DoodadGenerator.empty
    val pg = PickupGenerator.empty
    Stratum(ts, tg, fg, eg, dg, pg, sg)
  }

  def createAbyss(r:Random) = {
    val ts = TerrainScheme.generate(r)
    val sg = SpawnGenerator.dummy
    val tg = TerrainGenerator.dummy(ts)
    val fg = FeatureGenerator.abyss
    val eg = EntityGenerator.empty
    val dg = DoodadGenerator.empty
    val pg = PickupGenerator.empty
    Stratum(ts, tg, fg, eg, dg, pg, sg)
  }

  def apply[A](tsch:TerrainScheme,
               tgen:TerrainGenerator,
               fgen:FeatureGenerator[A],
               egen:EntityGenerator,
               dgen:DoodadGenerator,
               pgen:PickupGenerator,
               sgen:SpawnGenerator[A]) = {
    new Stratum {
      override type T = A
      override val ts: TerrainScheme = tsch
      override val dg: DoodadGenerator = dgen
      override val eg: EntityGenerator = egen
      override val pg: PickupGenerator = pgen
      override val sg: SpawnGenerator[T] = sgen
      override val fg: FeatureGenerator[T] = fgen
      override val tg: TerrainGenerator = tgen
    }
  }
}

trait Stratum {
  type T
  val ts:TerrainScheme
  val tg:TerrainGenerator
  val fg:FeatureGenerator[T]
  val eg:EntityGenerator
  val dg:DoodadGenerator
  val pg:PickupGenerator
  val sg:SpawnGenerator[T]
  val strataSize = 4

  def copy(ts:TerrainScheme=ts,
           tg:TerrainGenerator=tg,
           fg:FeatureGenerator[T]=fg,
           eg:EntityGenerator=eg,
           dg:DoodadGenerator=dg,
           pg:PickupGenerator=pg,
           sg:SpawnGenerator[T]=sg) = Stratum(ts, tg, fg, eg, dg, pg, sg)

  def modBiome(yIndex:Int, r:Random):Stratum = {
    val endIndex = 21
    if (yIndex > endIndex) {
      Stratum.createAbyss(r)
    } else if (yIndex == endIndex) {
      Stratum.createLair(r)
    } else if (yIndex == 1) {
      Stratum.createDummy(r)
    } else {
      if ((yIndex + 1) % strataSize == 0) {
        copy(ts=TerrainScheme.generate(r))
      } else {
        this
      }
    }
  }



  def generate(cols:Int, rows:Int, yIndex:Int, r:Random):(Stratum, Terrain, Seq[WorldSpawn], Seq[GlobalSpawn]) = {
    import Profundus._

    val (spawn, face, t) = sg.gen(cols, rows, r)
    val features = fg.assemble(cols, rows, yIndex, ts, r, t)
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
    val newBiome = modBiome(yIndex+1, r)
    (newBiome, Terrain(yIndex*rows, newTiles, doodads, spawn, face), Seq(entities) ++ pickups, gs)
  }

}
