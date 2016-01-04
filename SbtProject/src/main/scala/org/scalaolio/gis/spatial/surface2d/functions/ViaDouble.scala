/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.gis.spatial.surface2d.functions           **
**   Name:      ViaDouble.scala                                         **
**                                                                      **
** Description:                                                         **
**  Standard GIS spatial distance functions using Double (function      **
**  parameters are essentially untyped; i.e. weakly typed)              **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.gis.spatial.surface2d.functions

import scala.annotation.tailrec
import scala.reflect.runtime.universe.{TypeTag, typeTag}
import scala.util.{Failure, Success, Try}

import org.scalaolio.util.EnumerationDecorated
import org.scalaolio.util.FailedPreconditionsException
import org.scalaolio.util.FailedPreconditionsException.{FailedPrecondition, FailedPreconditionObject}

object ViaDouble {
  def epsilonDefault: Double =
    1.0E-14d //0.00000000000001

  def isWithinEpsilonDefault: (Double, Double) => Boolean =
    (double1, double2) =>
      Math.abs(double1 - double2) < Math.abs(epsilonDefault)

  object Haversine {
    val earthRadiusDefault: Double =
      6371014.3125d //meter

    private def calculateGreatCircleBearing(
        coordinatePair: ((Double, Double), (Double, Double)) //((radian, radian), (radian, radian))
    ): Double = { //radian
      val longitudeDiff =
        coordinatePair._2._1 - coordinatePair._1._1
      val y =
        Math.sin(longitudeDiff) * Math.cos(coordinatePair._2._2)
      val x = (
          (Math.cos(coordinatePair._1._2) * Math.sin(coordinatePair._2._2))
        - (Math.sin(coordinatePair._1._2) * Math.cos(coordinatePair._2._2) * Math.cos(longitudeDiff))
      )
      (Math.atan2(y, x) + (Math.PI * 2.0d)) % (Math.PI * 2.0d)
    }

    def calculateGreatCircleDistanceInitialBearingFinalBearing(
        coordinatePair: ((Double, Double), (Double, Double)) //((radian, radian), (radian, radian))
      , earthRadius: Double = earthRadiusDefault //meter
    ): (Double, Double, Double) = { //(meter, radian, radian)
      val longitudeDelta: Double =
        coordinatePair._2._1 - coordinatePair._1._1
      val latitude1: Double =
        coordinatePair._1._2
      val latitude2: Double =
        coordinatePair._2._2

      val d: Double = {
        val latitudeDelta =
          latitude2 - latitude1
        val a: Double = {
          val sinLongitudeDeltaInRadiansHalvedSquared = {
            val value =
              Math.sin(longitudeDelta / 2.0d)
            value * value
          }
          val sinLatitudeDeltaInRadiansHalvedSquared = {
            val value =
              Math.sin(latitudeDelta / 2.0d)
            value * value
          }
          (   sinLatitudeDeltaInRadiansHalvedSquared
            + (   Math.cos(latitude1) * Math.cos(latitude2)
                * sinLongitudeDeltaInRadiansHalvedSquared
              )
            )
        }
        val c =
          2.0d * Math.atan2(Math.sqrt(a), Math.sqrt(1.0d - a))
        earthRadius * c
      }
      (
          d
        , calculateGreatCircleBearing(coordinatePair)
        , calculateGreatCircleBearing(coordinatePair.swap)
      )
    }

    def calculateGreatCircleCoordinateAndFinalBearing(
        coordinate1: (Double, Double) //(radian, radian)
      , distance: Double //meter
      , bearingInitial: Double //radian
      , earthRadius: Double = earthRadiusDefault //meter
    ): ((Double, Double), Double) = { //((radian, radian), radian)
      val angularDistance = distance / earthRadius
      val latitude1 = coordinate1._2
      val latitude2 =
        Math.asin(
            (Math.sin(latitude1) * Math.cos(angularDistance))
          + (Math.cos(latitude1) * Math.sin(angularDistance) * Math.cos(bearingInitial))
        )
      val longitude2 = (
          coordinate1._1
        + Math.atan2(
              Math.sin(bearingInitial) * Math.sin(angularDistance) * Math.cos(latitude1)
            , Math.cos(angularDistance) - (Math.sin(latitude1) * Math.sin(latitude2))
          )
      )
      val coordinate2 =
        (longitude2, latitude2)
      (coordinate2, calculateGreatCircleBearing((coordinate2, coordinate1)))
    }

    private def calculatePsi(latitude: Double): Double =
      Math.tan((latitude / 2.0d) + (Math.PI / 4.0d))

    def calculateRhumbDistanceBearing(
        coordinatePair: ((Double, Double), (Double, Double)) //((radian, radian), (radian, radian))
      , earthRadius: Double = earthRadiusDefault //meter
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): (Double, Double) = { //(meter, radian)
      val deltaLongitude: Double = {
        val temp =
          coordinatePair._2._1 - coordinatePair._1._1
        if (Math.abs(temp) > Math.PI)
          ((if (temp > 0.0d) -2.0d else 2.0d) * Math.PI) + temp
        else
          temp
      }
      val latitude1: Double =
        coordinatePair._1._2
      val latitude2: Double =
        coordinatePair._2._2
      val deltaLatitude: Double =
        latitude2 - latitude1
      val psi1 =
        calculatePsi(latitude1)
      val psi2 =
        calculatePsi(latitude2)
      val deltaPsi =
        Math.log(psi2 / psi1)
      val q =
        if (isWithinEpsilon(deltaPsi, 0.0d))
          Math.cos(latitude1)
        else
          deltaLatitude / deltaPsi
      val angularDistanceInRadians =
        Math.sqrt((deltaLatitude * deltaLatitude) + ((q * q) * (deltaLongitude * deltaLongitude)))
      val bearing =
        Math.atan2(deltaLongitude, deltaPsi)
      (earthRadius * angularDistanceInRadians, (bearing + (Math.PI * 2)) % (Math.PI * 2))
    }

    def calculateRhumbCoordinate(
        coordinate1: (Double, Double) //(radian, radian)
      , distance: Double //meter
      , bearing: Double //radian
      , earthRadius: Double = earthRadiusDefault //meter
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): (Double, Double) = { //(radian, radian)
      val longitude1: Double =
        coordinate1._1
      val latitude1: Double =
        coordinate1._2
      val angularDistanceInRadians =
        distance / earthRadius
      val deltaLatitude =
        angularDistanceInRadians * Math.cos(bearing)
      val latitude2 = {
        val temp =
          latitude1 + deltaLatitude
        if (Math.abs(temp) > Math.PI / 2.0)
          (if (temp > 0.0d) Math.PI else -Math.PI) - temp
        else
          temp
      }
      val psi1 =
        calculatePsi(latitude1)
      val psi2 =
        calculatePsi(latitude2)
      val deltaPsi =
        Math.log(psi2 / psi1)
      val q =
        if (isWithinEpsilon(deltaPsi, 0.0d))
          Math.cos(latitude1)
        else
          deltaLatitude / deltaPsi
      val deltaLambda =
        angularDistanceInRadians * Math.sin(bearing) / q
      val longitude2Left =
        longitude1 + deltaLambda
      val longitude2 =
        ((longitude2Left + (3.0d * Math.PI)) % (2.0d * Math.PI)) - Math.PI
      (longitude2, latitude2)
    }
  }

  object Vincenty {
    final class EllisoidAxes(val major: Double, val minor: Double, val flatteningExplicit: Option[Double]) {
      val majorSquared: Double =
        major * major

      val minorSquared: Double =
        minor * minor

      val flatteningApproximated: Double =
        (major - minor) / major

      val flattening: Double =
        flatteningExplicit.getOrElse(flatteningApproximated)
    }

    object Ellipsoid extends EnumerationDecorated {
      case object WGS_84 extends Member
      case object GRS_80 extends Member
      case object AIRY_1830 extends Member
      case object INTERNATIONAL_1924 extends Member
      case object CLARK_MOD_1880 extends Member
      case object GRS_67 extends Member

      val decorationOrderedSet: List[Decoration] =
        List(
            Decoration(WGS_84,             "WGS-84", new EllisoidAxes(6378137.0d, 6356752.314245d, Some(1 / 298.257223563d)))
          , Decoration(GRS_80,             "GRS-80", new EllisoidAxes(6378137.0d, 6356752.314140d, Some(1 / 298.257222101d)))
          , Decoration(AIRY_1830,          "Airy 1830", new EllisoidAxes(6377563.396d, 6356256.909d, Some(1 / 299.3249646d)))
          , Decoration(INTERNATIONAL_1924, "Internat'l 1924", new EllisoidAxes(6378388.0d, 6356911.946d, Some(1 / 297.0d)))
          , Decoration(CLARK_MOD_1880,     "Clark mod.1880", new EllisoidAxes(6378249.145d, 6356514.86955d, Some(1 / 293.465d)))
          , Decoration(GRS_67,             "GRS-67", new EllisoidAxes(6378160.0d, 6356774.719d, Some(1 / 298.247167d)))
        )

      final case class Decoration private[Ellipsoid] (member: Member, datumName: String, ellisoidAxes: EllisoidAxes) extends DecorationBase {
        val diameterMajor: Double =
          2.0d * Math.PI * ellisoidAxes.major //TODO: must verify this is a sufficient formula

        val diameterMinor: Double =
          2.0d * Math.PI * ellisoidAxes.minor
      }

      override def typeTagMember: TypeTag[_] = typeTag[Member]
      sealed trait Member extends MemberDecorated
    }

    val ellipsoidDefault: Ellipsoid.Decoration =
      Ellipsoid.decorationByMember(Ellipsoid.WGS_84)

    object CalculationProfile extends ((Ellipsoid.Decoration, Int, Int, Double, Double) => CalculationProfile) {
      type Parameters =
        (Ellipsoid.Decoration, Int, Int, Double, Double)

      object FailedPreconditionCalculationProfile extends FailedPreconditionObject[FailedPreconditionCalculationProfile] {
        def apply(
            optionMessage: Option[String] = None
          , optionCause: Option[Throwable] = None
          , isEnableSuppression: Boolean = false
          , isWritableStackTrace: Boolean = false
        ): FailedPreconditionCalculationProfile =
          new FailedPreconditionCalculationProfile(
              optionMessage
            , optionCause
            , isEnableSuppression
            , isWritableStackTrace
          )
      }
      final class FailedPreconditionCalculationProfile private[FailedPreconditionCalculationProfile] (
          optionMessage: Option[String]
        , optionCause: Option[Throwable]
        , isEnableSuppression: Boolean
        , isWritableStackTrace: Boolean
      ) extends
        FailedPrecondition(
            optionMessage
          , optionCause
          , isEnableSuppression
          , isWritableStackTrace
        )

      val iterationDefaultForCalculateStartPointDistanceAndBearingDefault: Int = 200
      val iterationDefaultForCalculateCoordinatePairDefault: Int = 100
      val lambdaDeltaEpsilonDefault: Double = epsilonDefault
      val sinSigmaEpsilonDefault: Double = epsilonDefault

      val default: CalculationProfile =
        CalculationProfile()

      def apply(
          ellipsoid: Ellipsoid.Decoration =  ellipsoidDefault
        , iterationDefaultForCalculateStartPointDistanceAndBearing: Int = iterationDefaultForCalculateStartPointDistanceAndBearingDefault
        , iterationDefaultForCalculateCoordinatePair: Int = iterationDefaultForCalculateCoordinatePairDefault
        , lambdaDeltaEpsilon: Double = lambdaDeltaEpsilonDefault
        , sinSigmaEpsilon: Double = sinSigmaEpsilonDefault
      ): CalculationProfile =
        validatePreconditions(ellipsoid, iterationDefaultForCalculateStartPointDistanceAndBearing, iterationDefaultForCalculateCoordinatePair, lambdaDeltaEpsilon, sinSigmaEpsilon) match {
          case Some(failedPreconditionsException) =>
            throw failedPreconditionsException
          case None =>
            create(ellipsoid, iterationDefaultForCalculateStartPointDistanceAndBearing, iterationDefaultForCalculateCoordinatePair, lambdaDeltaEpsilon, sinSigmaEpsilon)
        }

      def apply(parameters: Parameters): CalculationProfile =
        CalculationProfile(parameters._1, parameters._2, parameters._3, parameters._4, parameters._5)

      def tryApply(
          ellipsoid: Ellipsoid.Decoration =  ellipsoidDefault
        , iterationDefaultForCalculateStartPointDistanceAndBearing: Int = iterationDefaultForCalculateStartPointDistanceAndBearingDefault
        , iterationDefaultForCalculateCoordinatePair: Int = iterationDefaultForCalculateCoordinatePairDefault
        , lambdaDeltaEpsilon: Double = lambdaDeltaEpsilonDefault
        , sinSigmaEpsilon: Double = sinSigmaEpsilonDefault
      ): Try[CalculationProfile] =
        validatePreconditions(ellipsoid, iterationDefaultForCalculateStartPointDistanceAndBearing, iterationDefaultForCalculateCoordinatePair, lambdaDeltaEpsilon, sinSigmaEpsilon) match {
          case Some(failedPreconditionsException) =>
            Failure(failedPreconditionsException)
          case None =>
            Success(create(ellipsoid, iterationDefaultForCalculateStartPointDistanceAndBearing, iterationDefaultForCalculateCoordinatePair, lambdaDeltaEpsilon, sinSigmaEpsilon))
        }

      def tryApply(parameters: Parameters): Try[CalculationProfile] =
        tryApply(parameters._1, parameters._2, parameters._3, parameters._4, parameters._5)

      def tryApplyFactory(
          ellipsoid: Ellipsoid.Decoration =  ellipsoidDefault
        , iterationDefaultForCalculateStartPointDistanceAndBearing: Int = iterationDefaultForCalculateStartPointDistanceAndBearingDefault
        , iterationDefaultForCalculateCoordinatePair: Int = iterationDefaultForCalculateCoordinatePairDefault
        , lambdaDeltaEpsilon: Double = lambdaDeltaEpsilonDefault
        , sinSigmaEpsilon: Double = sinSigmaEpsilonDefault
      ): Try[() => CalculationProfile] =
        validatePreconditions(ellipsoid, iterationDefaultForCalculateStartPointDistanceAndBearing, iterationDefaultForCalculateCoordinatePair, lambdaDeltaEpsilon, sinSigmaEpsilon) match {
          case Some(failedPreconditionsException) =>
            Failure(failedPreconditionsException)
          case None =>
            Success(
              new (() => CalculationProfile) {
                def apply(): CalculationProfile =
                  create(ellipsoid, iterationDefaultForCalculateStartPointDistanceAndBearing, iterationDefaultForCalculateCoordinatePair, lambdaDeltaEpsilon, sinSigmaEpsilon)
              }
            )
        }

      def tryApplyFactory(parameters: Parameters): Try[() => CalculationProfile] =
        tryApplyFactory(parameters._1, parameters._2, parameters._3, parameters._4, parameters._5)

      def validatePreconditions(
          ellipsoid: Ellipsoid.Decoration
        , iterationDefaultForCalculateStartPointDistanceAndBearing: Int
        , iterationDefaultForCalculateCoordinatePair: Int
        , lambdaDeltaEpsilon: Double
        , sinSigmaEpsilon: Double
      ): Option[FailedPreconditionsException] =
        FailedPreconditionsException.tryApply(clientValidatePreconditions(ellipsoid, iterationDefaultForCalculateStartPointDistanceAndBearing, iterationDefaultForCalculateCoordinatePair, lambdaDeltaEpsilon, sinSigmaEpsilon)).toOption

      def validatePreconditions(parameters: Parameters): Option[FailedPreconditionsException] =
        validatePreconditions(parameters._1, parameters._2, parameters._3, parameters._4, parameters._5)

      private def create(
          ellipsoid: Ellipsoid.Decoration
        , iterationDefaultForCalculateStartPointDistanceAndBearing: Int
        , iterationDefaultForCalculateCoordinatePair: Int
        , lambdaDeltaEpsilon: Double
        , sinSigmaEpsilon: Double
      ): CalculationProfile = {
        new CalculationProfile(ellipsoid, iterationDefaultForCalculateStartPointDistanceAndBearing, iterationDefaultForCalculateCoordinatePair, lambdaDeltaEpsilon, sinSigmaEpsilon) {
          private def readResolve(): Object =
            CalculationProfile(ellipsoid, iterationDefaultForCalculateStartPointDistanceAndBearing, iterationDefaultForCalculateCoordinatePair, lambdaDeltaEpsilon, sinSigmaEpsilon)

          def copy(
              ellipsoidNew: Ellipsoid.Decoration = ellipsoid
            , iterationDefaultForCalculateStartPointDistanceAndBearingNew: Int = iterationDefaultForCalculateStartPointDistanceAndBearing
            , iterationDefaultForCalculateCoordinatePairNew: Int = iterationDefaultForCalculateCoordinatePair
            , lambdaDeltaEpsilonNew: Double = lambdaDeltaEpsilon
            , sinSigmaEpsilonNew: Double = sinSigmaEpsilon
          ): CalculationProfile =
            CalculationProfile(ellipsoidNew, iterationDefaultForCalculateStartPointDistanceAndBearingNew, iterationDefaultForCalculateCoordinatePairNew, lambdaDeltaEpsilonNew, sinSigmaEpsilonNew)

          def tryCopy(
              ellipsoidNew: Ellipsoid.Decoration = ellipsoid
            , iterationDefaultForCalculateStartPointDistanceAndBearingNew: Int = iterationDefaultForCalculateStartPointDistanceAndBearing
            , iterationDefaultForCalculateCoordinatePairNew: Int = iterationDefaultForCalculateCoordinatePair
            , lambdaDeltaEpsilonNew: Double = lambdaDeltaEpsilon
            , sinSigmaEpsilonNew: Double = sinSigmaEpsilon
          ): Try[CalculationProfile] =
            CalculationProfile.tryApply(ellipsoidNew, iterationDefaultForCalculateStartPointDistanceAndBearingNew, iterationDefaultForCalculateCoordinatePairNew, lambdaDeltaEpsilonNew, sinSigmaEpsilonNew)

          def tryCopyFactory(
              ellipsoidNew: Ellipsoid.Decoration = ellipsoid
            , iterationDefaultForCalculateStartPointDistanceAndBearingNew: Int = iterationDefaultForCalculateStartPointDistanceAndBearing
            , iterationDefaultForCalculateCoordinatePairNew: Int = iterationDefaultForCalculateCoordinatePair
            , lambdaDeltaEpsilonNew: Double = lambdaDeltaEpsilon
            , sinSigmaEpsilonNew: Double = sinSigmaEpsilon
          ): Try[() => CalculationProfile] =
            CalculationProfile.tryApplyFactory(ellipsoidNew, iterationDefaultForCalculateStartPointDistanceAndBearingNew, iterationDefaultForCalculateCoordinatePairNew, lambdaDeltaEpsilonNew, sinSigmaEpsilonNew)

          def tuple =
            (ellipsoid, iterationDefaultForCalculateStartPointDistanceAndBearing, iterationDefaultForCalculateCoordinatePair, lambdaDeltaEpsilon, sinSigmaEpsilon)
        }
      }

      def clientValidatePreconditions(
          ellipsoid: Ellipsoid.Decoration
        , iterationDefaultForCalculateStartPointDistanceAndBearing: Int
        , iterationDefaultForCalculateCoordinatePair: Int
        , lambdaDeltaEpsilon: Double
        , sinSigmaEpsilon: Double
      ): List[FailedPrecondition] =
        List(
            if (!(iterationDefaultForCalculateStartPointDistanceAndBearing > 0))
              Some(FailedPreconditionCalculationProfile(s"iterationDefaultForCalculateStartPointDistanceAndBearing [$iterationDefaultForCalculateStartPointDistanceAndBearing] must be greater than 0"))
            else
              None
          , if (!(iterationDefaultForCalculateCoordinatePair > 0))
              Some(FailedPreconditionCalculationProfile(s"iterationDefaultForCalculateCoordinatePair [$iterationDefaultForCalculateCoordinatePair] must be greater than 0"))
            else
              None
          , if (!(lambdaDeltaEpsilon >= 0.0))
              Some(FailedPreconditionCalculationProfile(s"lambdaDeltaEpsilon [$lambdaDeltaEpsilon] must greater than or equal to 0.0d"))
            else
              None
          , if (!(sinSigmaEpsilon >= 0.0))
              Some(FailedPreconditionCalculationProfile(s"sinSigmaEpsilon [$sinSigmaEpsilon] must greater than or equal to 0.0d"))
            else
              None
        ).flatten
    }
    abstract case class CalculationProfile private[CalculationProfile] (
        ellipsoid: Ellipsoid.Decoration
      , iterationDefaultForCalculateStartPointDistanceAndBearing: Int
      , iterationDefaultForCalculateCoordinatePair: Int
      , lambdaDeltaEpsilon: Double
      , sinSigmaEpsilon: Double
    ) {
      def copy(
          ellipsoidNew: Ellipsoid.Decoration = ellipsoid
        , iterationDefaultForCalculateStartPointDistanceAndBearingNew: Int = iterationDefaultForCalculateStartPointDistanceAndBearing
        , iterationDefaultForCalculateCoordinatePairNew: Int = iterationDefaultForCalculateCoordinatePair
        , lambdaDeltaEpsilonNew: Double = lambdaDeltaEpsilon
        , sinSigmaEpsilonNew: Double = sinSigmaEpsilon
      ): CalculationProfile
      def tryCopy(
          ellipsoidNew: Ellipsoid.Decoration = ellipsoid
        , iterationDefaultForCalculateStartPointDistanceAndBearingNew: Int = iterationDefaultForCalculateStartPointDistanceAndBearing
        , iterationDefaultForCalculateCoordinatePairNew: Int = iterationDefaultForCalculateCoordinatePair
        , lambdaDeltaEpsilonNew: Double = lambdaDeltaEpsilon
        , sinSigmaEpsilonNew: Double = sinSigmaEpsilon
      ): Try[CalculationProfile]
      def tryCopyFactory(
          ellipsoidNew: Ellipsoid.Decoration = ellipsoid
        , iterationDefaultForCalculateStartPointDistanceAndBearingNew: Int = iterationDefaultForCalculateStartPointDistanceAndBearing
        , iterationDefaultForCalculateCoordinatePairNew: Int = iterationDefaultForCalculateCoordinatePair
        , lambdaDeltaEpsilonNew: Double = lambdaDeltaEpsilon
        , sinSigmaEpsilonNew: Double = sinSigmaEpsilon
      ): Try[() => CalculationProfile]
      def tuple: CalculationProfile.Parameters
    }

    private def calcTanCosSin(latitudeTheta: Double, calculationProfile: CalculationProfile): (Double, Double, Double) = {
      val tan: Double =
        (1.0d - calculationProfile.ellipsoid.ellisoidAxes.flattening) * Math.tan(latitudeTheta)
      val cos: Double =
        1.0d / Math.sqrt(1.0d + (tan * tan))
      val sin: Double =
        tan * cos
      (tan, cos, sin)
    }

    private def calc_A_B(cosSqAlpha: Double, calculationProfile: CalculationProfile): (Double, Double) = {
      val uSq =
        cosSqAlpha * (calculationProfile.ellipsoid.ellisoidAxes.majorSquared - calculationProfile.ellipsoid.ellisoidAxes.minorSquared) / calculationProfile.ellipsoid.ellisoidAxes.minorSquared
      (
          1.0d + (uSq / 16384.0d * (4096.0d + (uSq * (-768.0d + (uSq * (320.0d - (175.0d * uSq)))))))
        , (uSq / 1024.0d) * (256.0d + (uSq * (-128.0d + (uSq * (74.0d - (47.0d * uSq))))))
      )
    }

    private def calc_C(cosSqAlpha: Double, calculationProfile: CalculationProfile): Double =
      calculationProfile.ellipsoid.ellisoidAxes.flattening / 16.0d * cosSqAlpha * (4.0d + (calculationProfile.ellipsoid.ellisoidAxes.flattening * (4.0d - (3.0d * cosSqAlpha))))

    def calculateGreatCircleDistanceInitialBearingFinalBearing(
        coordinatePair: ((Double, Double), (Double, Double)) //((radian, radian), (radian, radian))
      , calculationProfile: CalculationProfile = CalculationProfile.default
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): Try[(Double, Double, Double)] = { //(meter, radian, radian)
      val coordinatePairResolve: ((Double, Double), (Double, Double)) = {
        //must push latitudes into 0.0d if the value isWithinEpsilon, otherwise the recursive algorithm won't converge
        val latitude10 = isWithinEpsilon(coordinatePair._1._2, 0.0d)
        val latitude20 = isWithinEpsilon(coordinatePair._2._2, 0.0d)
        if (latitude10 || latitude20)
          (
              if (latitude10)
                (coordinatePair._1._1, 0.0d)
              else
                coordinatePair._1
            , if (latitude20)
                (coordinatePair._2._1, 0.0d)
              else
                coordinatePair._2
          )
        else
          coordinatePair
      }
      val longitudeDelta: Double =
        coordinatePairResolve._2._1 - coordinatePairResolve._1._1
      val (_, cosU1, sinU1) =
        calcTanCosSin(coordinatePairResolve._1._2, calculationProfile)
      val (_, cosU2, sinU2) =
        calcTanCosSin(coordinatePairResolve._2._2, calculationProfile)

      @tailrec
      def recursive(index: Int, lambda: Double): Try[(Double, Double, Double, Double, Double, Double, Double)] = {
        //Failure
        //  - failed to start as the points were the same (less than epsilon - which should have been prevented by coordinatePair's construction conditions)
        //  - failed to converge
        //Success
        //  - Some((sinLambda, sinSigma, cosSigma, sigma, cosSqAlpha, cos2SigmaM))
        if (index == 0)
          Failure(new IllegalStateException(s"failed to converge within iteration limit [${calculationProfile.iterationDefaultForCalculateCoordinatePair}]"))
        else {
          val sinLambda = Math.sin(lambda)
          val cosLambda = Math.cos(lambda)
          val cosU2sinLambda =
            cosU2 * sinLambda
          val cosU2cosLambda =
            cosU2 * cosLambda
          val sinSqSigma = {
            val cosU1sinU2minussinU1cosU2cosLambda =
              (cosU1 * sinU2) - (sinU1 * cosU2cosLambda)
            (cosU2sinLambda * cosU2sinLambda) + (cosU1sinU2minussinU1cosU2cosLambda * cosU1sinU2minussinU1cosU2cosLambda)
          }
          val sinSigma =
            Math.sqrt(sinSqSigma)
          if (sinSigma < calculationProfile.sinSigmaEpsilon)
            Failure(new IllegalStateException(s"sinSigma [$sinSigma] is less than calculationProfile.sinSigmaEpsilon [${calculationProfile.sinSigmaEpsilon}]"))
          else {
            val sinU1sinU2 =
              sinU1 * sinU2
            val cosSigma = sinU1sinU2 + (cosU1 * cosU2cosLambda)
            val sigma = Math.atan2(sinSigma, cosSigma)
            val sinAlpha = cosU1 * cosU2sinLambda / sinSigma
            val cosSqAlpha = 1.0d - (sinAlpha * sinAlpha)
            val cos2SigmaM = {
              val value =
                cosSigma - (2.0d * sinU1sinU2 / cosSqAlpha)
              if (value.isNaN)
                0.0d //when isNaN is true, it indicates equatorial line; i.e. cosSqAlpha = 0.0d
              else
                value
            }
            val cC: Double =
              calc_C(cosSqAlpha, calculationProfile)
            val lambdaNew = (
                longitudeDelta
              + (   (1.0d - cC) * calculationProfile.ellipsoid.ellisoidAxes.flattening * sinAlpha
                  * (sigma + (cC * sinSigma * (cos2SigmaM + (cC * cosSigma * (-1.0d + (2.0d * cos2SigmaM * cos2SigmaM))))))
                )
            )
            if (Math.abs(lambdaNew - lambda) <= calculationProfile.lambdaDeltaEpsilon)
              Success((sigma, sinSigma, cosSigma, cos2SigmaM, sinLambda, cosLambda, cosSqAlpha))
            else
              recursive(index - 1, lambdaNew)
          }
        }
      }
      recursive(calculationProfile.iterationDefaultForCalculateCoordinatePair, longitudeDelta).map {
        case (sigma, sinSigma, cosSigma, cos2SigmaM, sinLambda, cosLambda, cosSqAlpha) =>
          val (aA, bB) =
            calc_A_B(cosSqAlpha, calculationProfile)
          val cos2SigmaMSquared =
            cos2SigmaM * cos2SigmaM
          val deltaSigma: Double = (
              bB * sinSigma
            * (   cos2SigmaM
                + (   bB / 4.0d
                    * (   (cosSigma * (-1.0d + (2.0d * cos2SigmaMSquared)))
                        - (   bB / 6.0d * cos2SigmaM
                            * (-3.0d + (4.0d * sinSigma * sinSigma))
                            * (-3.0d + (4.0d * cos2SigmaMSquared))
                          )
                      )
                  )
              )
          )
          val distance =
            calculationProfile.ellipsoid.ellisoidAxes.minor * aA * (sigma - deltaSigma)
          val angleInitial =
            Math.atan2(cosU2 * sinLambda, (cosU1 * sinU2) - (sinU1 * cosU2 * cosLambda))
          val angleFinal =
            Math.atan2(cosU1 * sinLambda, (-sinU1 * cosU2) + (cosU1 * sinU2 * cosLambda))
          (
              distance
            , (angleInitial + (Math.PI * 2.0d)) % (Math.PI * 2.0d)
            , (angleFinal + (Math.PI * 2.0d)) % (Math.PI * 2.0d)
          )
      }
    }

    def calculateGreatCircleCoordinateAndFinalBearing(
        coordinate1: (Double, Double) //(radian, radian)
      , distance: Double //meter
      , bearingInitial: Double //radian
      , calculationProfile: CalculationProfile = CalculationProfile.default
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): Try[((Double, Double), Double)] = { //((radian, radian), radian)
      val coordinate1Resolved =
        //must push latitude into 0.0d if the value isWithinEpsilon, otherwise the recursive algorithm won't converge
        if (isWithinEpsilon(coordinate1._2, 0.0d))
          (coordinate1._1, 0.0d)
        else
          coordinate1
      val (tanU1, cosU1, sinU1) =
        calcTanCosSin(coordinate1._2, calculationProfile)
      val sinAlpha1 = Math.sin(bearingInitial)
      val cosAlpha1 = Math.cos(bearingInitial)
      val sigma1 = Math.atan2(tanU1, cosAlpha1)
      val sinAlpha = cosU1 * sinAlpha1
      val sinAlphaSquared =
        sinAlpha * sinAlpha
      val cosSqAlpha = 1.0d - sinAlphaSquared
      val (aA, bB) =
        calc_A_B(cosSqAlpha, calculationProfile)
      @tailrec
      def recursive(index: Int, sigma: Double): Try[(Double, Double, Double, Double)] = {
        if (index == 0)
          Failure(new IllegalStateException(s"failed to converge within iteration limit [${calculationProfile.iterationDefaultForCalculateStartPointDistanceAndBearing}]"))
        else {
          val sinSigma =
            Math.sin(sigma)
          val cosSigma =
            Math.cos(sigma)
          val cos2SigmaM =
            Math.cos((2.0d * sigma1) + sigma)
          val cos2SigmaMSquared =
            cos2SigmaM * cos2SigmaM
          val deltaSigma: Double = (
              bB * sinSigma
            * (   cos2SigmaM
                + (   bB / 4.0d
                    * (   (cosSigma * (-1.0d + (2.0d * cos2SigmaMSquared)))
                        - (   bB / 6.0d * cos2SigmaM
                            * (-3.0d + (4.0d * sinSigma * sinSigma))
                            * (-3.0d + (4.0d * cos2SigmaMSquared))
                          )
                      )
                  )
              )
          )
          val sigmaNew =
            distance / (calculationProfile.ellipsoid.ellisoidAxes.minor * aA) + deltaSigma
          if (Math.abs(sigmaNew - sigma) < calculationProfile.sinSigmaEpsilon)
            Success((sigma, sinSigma, cosSigma, cos2SigmaM))
          else
            recursive(index - 1, sigmaNew)
        }
      }
      recursive(calculationProfile.iterationDefaultForCalculateStartPointDistanceAndBearing, distance / (calculationProfile.ellipsoid.ellisoidAxes.minor * aA)).flatMap {
        case (sigma, sinSigma, cosSigma, cos2SigmaM) =>
          val x =
            (sinU1 * sinSigma) - (cosU1 * cosSigma * cosAlpha1)
          val latitude2 =
            Math.atan2(
                (sinU1 * cosSigma) + (cosU1 * sinSigma * cosAlpha1)
              , (1.0d - calculationProfile.ellipsoid.ellisoidAxes.flattening) * Math.sqrt(sinAlphaSquared + (x * x))
            )
          val lambda =
            Math.atan2(
                sinSigma * sinAlpha1
              , (cosU1 * cosSigma) - (sinU1 * sinSigma * cosAlpha1)
            )
          val cC =
            calc_C(cosSqAlpha, calculationProfile)
          val lL = (
              lambda
            - (   (1.0d - cC) * calculationProfile.ellipsoid.ellisoidAxes.flattening * sinAlpha
                * (sigma + (cC * sinSigma * (cos2SigmaM + (cC * cosSigma * (-1.0d + (2.0d * cos2SigmaM * cos2SigmaM))))))
              )
          )
          val longitude2 =
            ((coordinate1._1 + lL + (3.0d * Math.PI)) % (2.0d * Math.PI)) - Math.PI
          val coordinate2 =
            (longitude2, latitude2)
          calculateGreatCircleDistanceInitialBearingFinalBearing((coordinate1, coordinate2)).flatMap(
            distanceAndInitialBearingAndFinalBearing =>
              Success((coordinate2, distanceAndInitialBearingAndFinalBearing._3))
          )
      }
    }

    private def calculatePsi(latitude: Double): Double = {
      Math.tan((latitude / 2.0d) + (Math.PI / 4.0d))
      //TODO: must get the ellipsoid version working (below) as the equation above is the simplified version from Haversine
      //val sinLatitude =
      //  Math.sin(latitude)
      //Math.tan((Math.PI / 4.0d) + (latitude / 2.0d)) * Math.pow((1 - (Math.E * sinLatitude)) / (1 + (Math.E * sinLatitude)), Math.E / 2.0d)
    }

    def calculateRhumbDistanceBearing(
        coordinatePair: ((Double, Double), (Double, Double)) //((radian, radian), (radian, radian))
      , calculationProfile: CalculationProfile = CalculationProfile.default
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): (Double, Double) = { //(meter, radian)
      val deltaLongitude: Double = {
        val temp =
          coordinatePair._2._1 - coordinatePair._1._1
        if (Math.abs(temp) > Math.PI)
          ((if (temp > 0.0d) -2.0d else 2.0d) * Math.PI) + temp
        else
          temp
      }
      val latitude1: Double =
        coordinatePair._1._2
      val latitude2: Double =
        coordinatePair._2._2
      val deltaLatitude: Double =
        latitude2 - latitude1
      val psi1 =
        calculatePsi(latitude1)
      val psi2 =
        calculatePsi(latitude2)
      val deltaPsi =
        Math.log(psi2 / psi1)
      val q =
        if (isWithinEpsilon(deltaPsi, 0.0d))
          Math.cos(latitude1)
        else
          deltaLatitude / deltaPsi
      val angularDistanceInRadians =
        Math.sqrt((deltaLatitude * deltaLatitude) + ((q * q) * (deltaLongitude * deltaLongitude)))
      val bearing =
        Math.atan2(deltaLongitude, deltaPsi)
      (calculationProfile.ellipsoid.ellisoidAxes.major * angularDistanceInRadians, (bearing + (Math.PI * 2)) % (Math.PI * 2))
    }

    def calculateRhumbCoordinate(
        coordinate1: (Double, Double) //(radian, radian)
      , distance: Double //meter
      , bearing: Double //radian
      , calculationProfile: CalculationProfile = CalculationProfile.default
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): (Double, Double) = { //(radian, radian)
      val longitude1: Double =
        coordinate1._1
      val latitude1: Double =
        coordinate1._2
      val angularDistanceInRadians =
        distance / calculationProfile.ellipsoid.ellisoidAxes.major
      val deltaLatitude =
        angularDistanceInRadians * Math.cos(bearing)
      val latitude2 = {
        val temp =
          latitude1 + deltaLatitude
        if (Math.abs(temp) > Math.PI / 2.0)
          (if (temp > 0.0d) Math.PI else -Math.PI) - temp
        else
          temp
      }
      val psi1 =
        calculatePsi(latitude1)
      val psi2 =
        calculatePsi(latitude2)
      val deltaPsi =
        Math.log(psi2 / psi1)
      val q =
        if (isWithinEpsilon(deltaPsi, 0.0d))
          Math.cos(latitude1)
        else
          deltaLatitude / deltaPsi
      val deltaLambda =
        angularDistanceInRadians * Math.sin(bearing) / q
      val longitude2Left =
        longitude1 + deltaLambda
      val longitude2 =
        ((longitude2Left + (3.0d * Math.PI)) % (2.0d * Math.PI)) - Math.PI
      (longitude2, latitude2)
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
