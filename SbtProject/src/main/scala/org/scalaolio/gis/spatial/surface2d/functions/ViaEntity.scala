/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.gis.spatial.surface2d.functions           **
**   Name:      ViaEntity.scala                                         **
**                                                                      **
** Description:                                                         **
**  Standard GIS spatial distance functions using specific classes to   **
**  explicitly limit domains for function parameters; i.e. very         **
**  strongly typed)                                                     **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.gis.spatial.surface2d.functions

import scala.util.Try

import org.scalaolio.gis.spatial.surface2d.entity._
import org.scalaolio.gis.spatial.surface2d.functions.ViaDouble.Vincenty.CalculationProfile
import squants._

object ViaEntity {
  def epsilonDefault: Double =
    ViaDouble.epsilonDefault

  def isWithinEpsilonDefault: (Double, Double) => Boolean =
    (double1, double2) =>
      Math.abs(double1 - double2) < Math.abs(epsilonDefault)

  trait Model {

  }

  object Haversine {
    val earthRadiusDefault: Distance =
      Distance(Meters(ViaDouble.Haversine.earthRadiusDefault))

    def calculateGreatCircleDistanceInitialBearingFinalBearing(
        coordinatePair: (Coordinate, Coordinate)
      , earthRadius: Distance = earthRadiusDefault
    ): (Distance, Bearing, Bearing) = {
      val (distanceInMeter, bearingInitialInRadian, bearingFinalInRadian) =
        ViaDouble.Haversine.calculateGreatCircleDistanceInitialBearingFinalBearing(
            (   (coordinatePair._1.longitude.angle.toRadians, coordinatePair._1.latitude.angle.toRadians)
              , (coordinatePair._2.longitude.angle.toRadians, coordinatePair._2.latitude.angle.toRadians)
            )
          , earthRadius.length.toMeters
        )
      (Distance(Meters(distanceInMeter)), Bearing(Radians(bearingInitialInRadian)), Bearing(Radians(bearingFinalInRadian)))
    }

    def calculateGreatCircleCoordinateAndFinalBearing(
        coordinate1: Coordinate
      , distance: Distance
      , bearingInitial: Bearing
      , earthRadius: Distance = earthRadiusDefault
    ): (Coordinate, Bearing) = {
      val ((longitudeInRadians, latitudeInRadians), bearingFinalInRadian) =
        ViaDouble.Haversine.calculateGreatCircleCoordinateAndFinalBearing(
            (coordinate1.longitude.angle.toRadians, coordinate1.latitude.angle.toRadians)
          , distance.length.toMeters
          , bearingInitial.angle.toRadians
          , earthRadius.length.toMeters
        )
      (Coordinate(Longitude(Radians(longitudeInRadians)), Latitude(Radians(latitudeInRadians))), Bearing(Radians(bearingFinalInRadian)))
    }

    def calculateRhumbDistanceBearing(
        coordinatePair: (Coordinate, Coordinate)
      , earthRadius: Distance = earthRadiusDefault
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): (Distance, Bearing) = {
      val (distanceInMeter, bearingInRadian) =
        ViaDouble.Haversine.calculateRhumbDistanceBearing(
            (   (coordinatePair._1.longitude.angle.toRadians, coordinatePair._1.latitude.angle.toRadians)
              , (coordinatePair._2.longitude.angle.toRadians, coordinatePair._2.latitude.angle.toRadians)
            )
          , earthRadius.length.toMeters
          , isWithinEpsilon
        )
      (Distance(Meters(distanceInMeter)), Bearing(Radians(bearingInRadian)))
    }

    def calculateRhumbCoordinate(
        coordinate1: Coordinate
      , distance: Distance
      , bearing: Bearing
      , earthRadius: Distance = earthRadiusDefault
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): Coordinate = {
      val (longitudeInRadians, latitudeInRadians) =
        ViaDouble.Haversine.calculateRhumbCoordinate(
            (coordinate1.longitude.angle.toRadians, coordinate1.latitude.angle.toRadians)
          , distance.length.toMeters
          , bearing.angle.toRadians
          , earthRadius.length.toMeters
          , isWithinEpsilon
        )
      Coordinate(Longitude(Radians(longitudeInRadians)), Latitude(Radians(latitudeInRadians)))
    }
  }

  object Vincenty {
    val calculationProfileDefault: CalculationProfile =
      CalculationProfile.default

    def calculateGreatCircleDistanceInitialBearingFinalBearing(
        coordinatePair: (Coordinate, Coordinate)
      , calculationProfile: CalculationProfile = calculationProfileDefault
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): Try[(Distance, Bearing, Bearing)] =
      ViaDouble.Vincenty.calculateGreatCircleDistanceInitialBearingFinalBearing(
          (   (coordinatePair._1.longitude.angle.toRadians, coordinatePair._1.latitude.angle.toRadians)
            , (coordinatePair._2.longitude.angle.toRadians, coordinatePair._2.latitude.angle.toRadians)
          )
        , calculationProfile
        , isWithinEpsilon
      ).map {
        case (distanceInMeter, bearingInitialInRadian, bearingFinalInRadian) =>
          (Distance(Meters(distanceInMeter)), Bearing(Radians(bearingInitialInRadian)), Bearing(Radians(bearingFinalInRadian)))
      }

    def calculateGreatCircleCoordinateAndFinalBearing(
        coordinate1: Coordinate
      , distance: Distance
      , bearingInitial: Bearing
      , calculationProfile: CalculationProfile = CalculationProfile.default
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): Try[(Coordinate, Bearing)] =
      ViaDouble.Vincenty.calculateGreatCircleCoordinateAndFinalBearing(
          (coordinate1.longitude.angle.toRadians, coordinate1.latitude.angle.toRadians)
        , distance.length.toMeters
        , bearingInitial.angle.toRadians
        , calculationProfile
        , isWithinEpsilon
      ).map {
        case ((longitudeInRadians, latitudeInRadians), bearingFinalInRadian) =>
          (Coordinate(Longitude(Radians(longitudeInRadians)), Latitude(Radians(latitudeInRadians))), Bearing(Radians(bearingFinalInRadian)))
      }

    def calculateRhumbDistanceBearing(
        coordinatePair: (Coordinate, Coordinate)
      , calculationProfile: CalculationProfile = CalculationProfile.default
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): (Distance, Bearing) = {
      val (distanceInMeter, bearingInRadian) =
        ViaDouble.Vincenty.calculateRhumbDistanceBearing(
            (   (coordinatePair._1.longitude.angle.toRadians, coordinatePair._1.latitude.angle.toRadians)
              , (coordinatePair._2.longitude.angle.toRadians, coordinatePair._2.latitude.angle.toRadians)
            )
          , calculationProfile
          , isWithinEpsilon
        )
      (Distance(Meters(distanceInMeter)), Bearing(Radians(bearingInRadian)))
    }

    def calculateRhumbCoordinate(
        coordinate1: Coordinate
      , distance: Distance
      , bearing: Bearing
      , calculationProfile: CalculationProfile = CalculationProfile.default
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): Coordinate = {
      val (longitudeInRadians, latitudeInRadians) =
        ViaDouble.Vincenty.calculateRhumbCoordinate(
            (coordinate1.longitude.angle.toRadians, coordinate1.latitude.angle.toRadians)
          , distance.length.toMeters
          , bearing.angle.toRadians
          , calculationProfile
          , isWithinEpsilon
        )
      Coordinate(Longitude(Radians(longitudeInRadians)), Latitude(Radians(latitudeInRadians)))
    }
  }
}
/*
This Scala file is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

To see details of the GPLv3 License, please see
<http://www.gnu.org/copyleft/gpl.html>.
To see details of the GNU General Public License, please see
<http://www.gnu.org/licenses/>.

If you would like to obtain a custom/different/commercial license for
this, please send an email with your request to
<jim.oflaherty.jr@gmail.com>.
*/
