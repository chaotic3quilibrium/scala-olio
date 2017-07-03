package org.scalaolio.gis.spatial.surface2d.geohash

import org.scalatest._

class GeohashSpec extends FlatSpec with Matchers {
  "An explicit value of '0'" should "return a valid Geohash" in {
    val geohash =
      Geohash("0")

    geohash.value should be ("0")
    geohash.boundsLowerLeft.longitude.angle.value should be (-180.0)
    geohash.boundsLowerLeft.latitude.angle.value should be (-90.0)
    geohash.boundsUpperRight.longitude.angle.value should be (-135.0)
    geohash.boundsUpperRight.latitude.angle.value should be (-45.0)
  }

  "An explicit value of '2'" should "return a valid Geohash" in {
    val geohash =
      Geohash("2")

    geohash.value should be ("2")
    geohash.boundsLowerLeft.longitude.angle.value should be (-180.0)
    geohash.boundsLowerLeft.latitude.angle.value should be (-45.0)
    geohash.boundsUpperRight.longitude.angle.value should be (-135.0)
    geohash.boundsUpperRight.latitude.angle.value should be (0.0)
  }

  "An explicit value of '8'" should "return a valid Geohash" in {
    val geohash =
      Geohash("8")

    geohash.value should be ("8")
    geohash.boundsLowerLeft.longitude.angle.value should be (-180.0)
    geohash.boundsLowerLeft.latitude.angle.value should be (0.0)
    geohash.boundsUpperRight.longitude.angle.value should be (-135.0)
    geohash.boundsUpperRight.latitude.angle.value should be (45.0)
  }

  "An explicit value of 'b'" should "return a valid Geohash" in {
    val geohash =
      Geohash("b")

    geohash.value should be ("b")
    geohash.boundsLowerLeft.longitude.angle.value should be (-180.0)
    geohash.boundsLowerLeft.latitude.angle.value should be (45.0)
    geohash.boundsUpperRight.longitude.angle.value should be (-135.0)
    geohash.boundsUpperRight.latitude.angle.value should be (90.0)
  }

  "An explicit value of 'p'" should "return a valid Geohash" in {
    val geohash =
      Geohash("p")

    geohash.value should be ("p")
    geohash.boundsLowerLeft.longitude.angle.value should be (135.0)
    geohash.boundsLowerLeft.latitude.angle.value should be (-90.0)
    geohash.boundsUpperRight.longitude.angle.value should be (180.0)
    geohash.boundsUpperRight.latitude.angle.value should be (-45.0)
  }

  "An explicit value of 'r'" should "return a valid Geohash" in {
    val geohash =
      Geohash("r")

    geohash.value should be ("r")
    geohash.boundsLowerLeft.longitude.angle.value should be (135.0)
    geohash.boundsLowerLeft.latitude.angle.value should be (-45.0)
    geohash.boundsUpperRight.longitude.angle.value should be (180.0)
    geohash.boundsUpperRight.latitude.angle.value should be (-0.0)
  }

  "An explicit value of 'x'" should "return a valid Geohash" in {
    val geohash =
      Geohash("x")

    geohash.value should be ("x")
    geohash.boundsLowerLeft.longitude.angle.value should be (135.0)
    geohash.boundsLowerLeft.latitude.angle.value should be (0.0)
    geohash.boundsUpperRight.longitude.angle.value should be (180.0)
    geohash.boundsUpperRight.latitude.angle.value should be (45.0)
  }

  "An explicit value of 'z'" should "return a valid Geohash" in {
    val geohash =
      Geohash("z")

    geohash.value should be ("z")
    geohash.boundsLowerLeft.longitude.angle.value should be (135.0)
    geohash.boundsLowerLeft.latitude.angle.value should be (45.0)
    geohash.boundsUpperRight.longitude.angle.value should be (180.0)
    geohash.boundsUpperRight.latitude.angle.value should be (90.0)
  }
}
