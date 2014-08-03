package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.procgen.PerlinNoise
import in.dogue.profundus.Profundus
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.profundus.entities.pickups.FoodFactory

object Stratum {
  def createDummy(r:Random, ts:TerrainScheme) = {
    val tg = TerrainGenerator.dummy
    val fg = FeatureGenerator.dummy
    val eg = EntityGenerator.dummy
    val dg = DoodadGenerator.empty
    val pg = PickupGenerator.dummy
    val sg = SpawnGenerator.dummy
    val mod = TerrainModGroup.spikes(1000)
    Stratum(Seq(), ts, tg, fg, eg, dg, pg, sg, mod)
  }

  def createSurface(r:Random) = {
    val ts = TerrainScheme.generate(r)
    val sg = SpawnGenerator.surface
    val tg = TerrainGenerator.dummy
    val fg = FeatureGenerator.surface
    val eg = EntityGenerator.empty
    val dg = DoodadGenerator.empty
    val pg = PickupGenerator.empty
    val mod = TerrainModGroup.empty
    Stratum(Seq(), ts, tg, fg, eg, dg, pg, sg, mod)
  }

  def createLair(r:Random):Stratum = {
    val ts = TerrainScheme.generate(r)
    val sg = SpawnGenerator.dummy
    val tg = TerrainGenerator.dummy
    val fg = FeatureGenerator.lair
    val eg = EntityGenerator.lair
    val dg = DoodadGenerator.empty
    val pg = PickupGenerator.empty
    val mod = TerrainModGroup.empty
    Stratum(Seq(), ts, tg, fg, eg, dg, pg, sg, mod)
  }

  def createAbyss(r:Random) = {
    val ts = TerrainScheme.generate(r)
    val sg = SpawnGenerator.dummy
    val tg = TerrainGenerator.dummy
    val fg = FeatureGenerator.abyss
    val eg = EntityGenerator.empty
    val dg = DoodadGenerator.empty
    val pg = PickupGenerator.empty
    val mod = TerrainModGroup.empty
    Stratum(Seq(), ts, tg, fg, eg, dg, pg, sg, mod)
  }

  def apply[A](anext:Seq[Feature],
               tsch:TerrainScheme,
               tgen:TerrainGenerator,
               fgen:FeatureGenerator[A],
               egen:EntityGenerator,
               dgen:DoodadGenerator,
               pgen:PickupGenerator,
               sgen:SpawnGenerator[A],
               mod_ :TerrainModGroup) = {
    new Stratum {
      override type T = A
      override val next = anext
      override val ts = tsch
      override val dg = dgen
      override val eg = egen
      override val pg = pgen
      override val sg = sgen
      override val fg = fgen
      override val tg = tgen
      override val mod = mod_
    }
  }
}

trait Stratum {
  type T
  val next:Seq[Feature]
  val ts:TerrainScheme
  val tg:TerrainGenerator
  val fg:FeatureGenerator[T]
  val eg:EntityGenerator
  val dg:DoodadGenerator
  val pg:PickupGenerator
  val sg:SpawnGenerator[T]
  val mod:TerrainModGroup
  val strataSize = 4

  def copy(next:Seq[Feature]=next,
           ts:TerrainScheme=ts,
           tg:TerrainGenerator=tg,
           fg:FeatureGenerator[T]=fg,
           eg:EntityGenerator=eg,
           dg:DoodadGenerator=dg,
           pg:PickupGenerator=pg,
           sg:SpawnGenerator[T]=sg,
           mod:TerrainModGroup=mod) = Stratum(next, ts, tg, fg, eg, dg, pg, sg, mod)

  def modBiome(yIndex:Int, r:Random):Stratum = {
    val endIndex = 21
    val nn = if (yIndex > endIndex) {
      Stratum.createAbyss(r)
    } else if (yIndex == endIndex) {
      Stratum.createLair(r)
    } else if (yIndex >= 0) {
      val newTs = if (yIndex %% strataSize == 0) {
        TerrainScheme.generate(r)
      } else {
        this.ts
      }
      Stratum.createDummy(r, newTs)
    } else {
      this
    }
    nn.copy(next=next)
  }

  def withNext(n:Seq[Feature]) = copy(next=n)


  def generate(cols:Int, rows:Int, yIndex:Int, r:Random):(Terrain, Seq[GlobalMessage], Seq[Feature]) = {
    import Profundus._
    val (spawn, face, t) = sg.gen(cols, rows, r)
    val features = fg.assemble(next, cols, rows, yIndex, ts, r, t)
    val noise = new PerlinNoise().generate(cols, rows, 0, yIndex, r.nextInt())
    val tf = ts.toFactory(r)
    val (nt, gen) = noise.map { case (ij, d) =>
      tg.generate(ts, tf, ij, yIndex, cols, rows, d, r)
    }.unzip
    val (tiles, pgens) = mod.mod(tf, yIndex*rows, Terrain.merge(nt, gen), r)

    val (newTiles, gs, fts) = fold3(tiles, features) { case (ft, terrain) =>
      ft.transform(cols, rows, yIndex * rows, ts, terrain, r)
    }
    val ff = FoodFactory.create(r)//fixme -- should have its own generator?
    val pickups = pg.generate(ff, cols, rows, yIndex*rows, newTiles, ts, r)

    val doodads = dg.generate(ts, newTiles, r).gms
    val entities = eg.generate(cols, rows, yIndex, ts, newTiles, r)
    (Terrain(yIndex*rows, tf, newTiles, spawn, face), gs ++ Seq(entities) ++ pickups ++ doodads ++ pgens, fts)
  }

}
