package org.scalaolio.gis.spatial.surface2d.geohash

import org.scalaolio.gis.spatial.surface2d.entity.{Coordinate, Direction}
import org.scalatest._

class GeohashOpsSpec extends FlatSpec with Matchers {
  "A Geohash of '0'" should "return a valid RichGeohash" in {
    val richGeohash =
      GeohashOps.RichGeohash(Geohash("0"))

    richGeohash.geohash.value should be ("0")
    richGeohash.boundsLowerRight should be (Coordinate(richGeohash.geohash.boundsUpperRight.longitude, richGeohash.geohash.boundsLowerLeft.latitude))
    richGeohash.boundsUpperLeft should be (Coordinate(richGeohash.geohash.boundsLowerLeft.longitude, richGeohash.geohash.boundsUpperRight.latitude))
    richGeohash.isAdjacentsComplete should be (false)
    richGeohash.adjacents should be (
      Map(
          Direction.NORTH -> Geohash("2")
        , Direction.NORTH_EAST -> Geohash("3")
        , Direction.EAST -> Geohash("1")
//        , Direction.SOUTH_EAST -> Geohash("1")
//        , Direction.SOUTH -> Geohash("1")
//        , Direction.SOUTH_WEST -> Geohash("1")
        , Direction.WEST -> Geohash("p")
        , Direction.NORTH_WEST -> Geohash("r")
      )
    )
  }

  "A Geohash of '2'" should "return a valid RichGeohash" in {
    val richGeohash =
      GeohashOps.RichGeohash(Geohash("2"))

    richGeohash.geohash.value should be ("2")
    richGeohash.boundsLowerRight should be (Coordinate(richGeohash.geohash.boundsUpperRight.longitude, richGeohash.geohash.boundsLowerLeft.latitude))
    richGeohash.boundsUpperLeft should be (Coordinate(richGeohash.geohash.boundsLowerLeft.longitude, richGeohash.geohash.boundsUpperRight.latitude))
    richGeohash.isAdjacentsComplete should be (true)
    richGeohash.adjacents should be (
      Map(
          Direction.NORTH -> Geohash("8")
        , Direction.NORTH_EAST -> Geohash("9")
        , Direction.EAST -> Geohash("3")
        , Direction.SOUTH_EAST -> Geohash("1")
        , Direction.SOUTH -> Geohash("0")
        , Direction.SOUTH_WEST -> Geohash("p")
        , Direction.WEST -> Geohash("r")
        , Direction.NORTH_WEST -> Geohash("x")
      )
    )
  }

  "A Geohash of '8'" should "return a valid RichGeohash" in {
    val richGeohash =
      GeohashOps.RichGeohash(Geohash("8"))

    richGeohash.geohash.value should be ("8")
    richGeohash.boundsLowerRight should be (Coordinate(richGeohash.geohash.boundsUpperRight.longitude, richGeohash.geohash.boundsLowerLeft.latitude))
    richGeohash.boundsUpperLeft should be (Coordinate(richGeohash.geohash.boundsLowerLeft.longitude, richGeohash.geohash.boundsUpperRight.latitude))
    richGeohash.isAdjacentsComplete should be (true)
    richGeohash.adjacents should be (
      Map(
          Direction.NORTH -> Geohash("b")
        , Direction.NORTH_EAST -> Geohash("c")
        , Direction.EAST -> Geohash("9")
        , Direction.SOUTH_EAST -> Geohash("3")
        , Direction.SOUTH -> Geohash("2")
        , Direction.SOUTH_WEST -> Geohash("r")
        , Direction.WEST -> Geohash("x")
        , Direction.NORTH_WEST -> Geohash("z")
      )
    )
  }

  "A Geohash of 'b'" should "return a valid RichGeohash" in {
    val richGeohash =
      GeohashOps.RichGeohash(Geohash("b"))

    richGeohash.geohash.value should be ("b")
    richGeohash.boundsLowerRight should be (Coordinate(richGeohash.geohash.boundsUpperRight.longitude, richGeohash.geohash.boundsLowerLeft.latitude))
    richGeohash.boundsUpperLeft should be (Coordinate(richGeohash.geohash.boundsLowerLeft.longitude, richGeohash.geohash.boundsUpperRight.latitude))
    richGeohash.isAdjacentsComplete should be (false)
    richGeohash.adjacents should be (
      Map(
//          Direction.NORTH -> Geohash("2")
//        , Direction.NORTH_EAST -> Geohash("3")
          Direction.EAST -> Geohash("c")
        , Direction.SOUTH_EAST -> Geohash("9")
        , Direction.SOUTH -> Geohash("8")
        , Direction.SOUTH_WEST -> Geohash("x")
        , Direction.WEST -> Geohash("z")
//        , Direction.NORTH_WEST -> Geohash("r")
      )
    )
  }

  "A Geohash of 'p'" should "return a valid RichGeohash" in {
    val richGeohash =
      GeohashOps.RichGeohash(Geohash("p"))

    richGeohash.geohash.value should be ("p")
    richGeohash.boundsLowerRight should be (Coordinate(richGeohash.geohash.boundsUpperRight.longitude, richGeohash.geohash.boundsLowerLeft.latitude))
    richGeohash.boundsUpperLeft should be (Coordinate(richGeohash.geohash.boundsLowerLeft.longitude, richGeohash.geohash.boundsUpperRight.latitude))
    richGeohash.isAdjacentsComplete should be (false)
    richGeohash.adjacents should be (
      Map(
          Direction.NORTH -> Geohash("r")
        , Direction.NORTH_EAST -> Geohash("2")
        , Direction.EAST -> Geohash("0")
//        , Direction.SOUTH_EAST -> Geohash("1")
//        , Direction.SOUTH -> Geohash("1")
//        , Direction.SOUTH_WEST -> Geohash("1")
        , Direction.WEST -> Geohash("n")
        , Direction.NORTH_WEST -> Geohash("q")
      )
    )
  }

  "A Geohash of 'r'" should "return a valid RichGeohash" in {
    val richGeohash =
      GeohashOps.RichGeohash(Geohash("r"))

    richGeohash.geohash.value should be ("r")
    richGeohash.boundsLowerRight should be (Coordinate(richGeohash.geohash.boundsUpperRight.longitude, richGeohash.geohash.boundsLowerLeft.latitude))
    richGeohash.boundsUpperLeft should be (Coordinate(richGeohash.geohash.boundsLowerLeft.longitude, richGeohash.geohash.boundsUpperRight.latitude))
    richGeohash.isAdjacentsComplete should be (true)
    richGeohash.adjacents should be (
      Map(
          Direction.NORTH -> Geohash("x")
        , Direction.NORTH_EAST -> Geohash("8")
        , Direction.EAST -> Geohash("2")
        , Direction.SOUTH_EAST -> Geohash("0")
        , Direction.SOUTH -> Geohash("p")
        , Direction.SOUTH_WEST -> Geohash("n")
        , Direction.WEST -> Geohash("q")
        , Direction.NORTH_WEST -> Geohash("w")
      )
    )
  }

  "A Geohash of 'x'" should "return a valid RichGeohash" in {
    val richGeohash =
      GeohashOps.RichGeohash(Geohash("x"))

    richGeohash.geohash.value should be ("x")
    richGeohash.boundsLowerRight should be (Coordinate(richGeohash.geohash.boundsUpperRight.longitude, richGeohash.geohash.boundsLowerLeft.latitude))
    richGeohash.boundsUpperLeft should be (Coordinate(richGeohash.geohash.boundsLowerLeft.longitude, richGeohash.geohash.boundsUpperRight.latitude))
    richGeohash.isAdjacentsComplete should be (true)
    richGeohash.adjacents should be (
      Map(
          Direction.NORTH -> Geohash("z")
        , Direction.NORTH_EAST -> Geohash("b")
        , Direction.EAST -> Geohash("8")
        , Direction.SOUTH_EAST -> Geohash("2")
        , Direction.SOUTH -> Geohash("r")
        , Direction.SOUTH_WEST -> Geohash("q")
        , Direction.WEST -> Geohash("w")
        , Direction.NORTH_WEST -> Geohash("y")
      )
    )
  }

  "A Geohash of 'z'" should "return a valid RichGeohash" in {
    val richGeohash =
      GeohashOps.RichGeohash(Geohash("z"))

    richGeohash.geohash.value should be ("z")
    richGeohash.boundsLowerRight should be (Coordinate(richGeohash.geohash.boundsUpperRight.longitude, richGeohash.geohash.boundsLowerLeft.latitude))
    richGeohash.boundsUpperLeft should be (Coordinate(richGeohash.geohash.boundsLowerLeft.longitude, richGeohash.geohash.boundsUpperRight.latitude))
    richGeohash.isAdjacentsComplete should be (false)
    richGeohash.adjacents should be (
      Map(
//          Direction.NORTH -> Geohash("2")
//        , Direction.NORTH_EAST -> Geohash("3")
          Direction.EAST -> Geohash("b")
        , Direction.SOUTH_EAST -> Geohash("8")
        , Direction.SOUTH -> Geohash("x")
        , Direction.SOUTH_WEST -> Geohash("w")
        , Direction.WEST -> Geohash("y")
//        , Direction.NORTH_WEST -> Geohash("r")
      )
    )
  }
}
