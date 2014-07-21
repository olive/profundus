package in.dogue.profundus.world

import scala.util.Random
import in.dogue.antiqua.procgen.PerlinNoise
import in.dogue.profundus.Profundus
import in.dogue.antiqua.data.{Array2d, Direction}
import in.dogue.profundus.doodads.Doodad
import com.deweyvm.gleany.data.Recti
import in.dogue.profundus.entities.pickups.ToolPickup
import in.dogue.profundus.entities.{Gouge, BareHands, Shovel}
import in.dogue.antiqua.algebra.Monoid

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
}

case class Stratum(ts:TerrainScheme, tg:TerrainGenerator, fg:FeatureGenerator, eg:EntityGenerator, dg:DoodadGenerator, pg:PickupGenerator) {
  val strataSize = 4
  def generate(cols:Int, rows:Int, yIndex:Int, r:Random):(Stratum, Terrain, Seq[WorldSpawn], Seq[GlobalSpawn]) = {
    import Profundus._
    val (spawn, face, features) = if (yIndex < 0) {
      ((0,0), Direction.Down, Seq(Terrain.skyFeature(cols, rows)))
    } else if (yIndex == 0) {
      val (spawn, face, lines, circle) = Terrain.makeLines(cols, rows, r)//createMouth
      val f = Feature(Recti(0,0,cols,rows), Terrain.createMouth(face, lines, circle))
      (spawn, face, Seq(f))
    } else {
      ((0,0), Direction.Down, fg.assemble(cols, rows, yIndex, ts, r))
    }
    val noise = new PerlinNoise().generate(cols, rows, 0, yIndex, r.nextInt())
    val tiles = noise.map { case (i, j, d) =>
      val state = tg.mkTile(ts, i, j, yIndex, cols, rows, d, r)
      WorldTile(state(r))
    }


    val seed = (tiles, Seq[Doodad[_]](), Seq[GlobalSpawn]())
    val (newTiles, ds, gs) = fold3(seed, features) { case (ft, tiles) =>
      ft.transform(cols, rows, yIndex * rows, ts, tiles, r)
    }


    val pickups = {
      if (yIndex > 0) {
        pg.generate(cols, rows, yIndex*rows, newTiles, ts, r)
      } else {
        Seq(Seq(ToolPickup.create((75, 16), Gouge.toTool)).ws)
      }
    }
    val doodads = dg.generate(ts, newTiles, r) ++ ds
    val entities = eg.generate(cols, rows, yIndex, newTiles, r)
    val newBiome = if (yIndex % strataSize == 0) {
      copy(ts=TerrainScheme.generate(r))
    } else {
      this
    }
    (newBiome, Terrain(yIndex*rows, newTiles, doodads, spawn, face), Seq(entities) ++ pickups, gs)
  }
}
