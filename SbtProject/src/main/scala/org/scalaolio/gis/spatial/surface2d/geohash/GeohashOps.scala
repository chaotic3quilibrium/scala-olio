/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.gis.spatial.surface2d.geohash             **
**   Name:      GeohashOps.scala                                        **
**                                                                      **
** Description:                                                         **
**  Extensions to Geohash facilitating normal uses cases                **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.gis.spatial.surface2d.geohash

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

import org.scalaolio.gis.spatial.surface2d.entity.{Coordinate, Direction, Latitude, Longitude}
import org.scalaolio.gis.spatial.surface2d.functions.ViaEntity
import org.scalaolio.util.FailedPreconditionsException.{FailedPrecondition, FailedPreconditionObject}
import squants._

object GeohashOps {
  object FailedPreconditionGeohashOps extends FailedPreconditionObject[FailedPreconditionGeohashOps] {
    def apply(
        optionMessage: Option[String] = None
      , optionCause: Option[Throwable] = None
      , isEnableSuppression: Boolean = false
      , isWritableStackTrace: Boolean = false
    ): FailedPreconditionGeohashOps =
      new FailedPreconditionGeohashOps(
          optionMessage
        , optionCause
        , isEnableSuppression
        , isWritableStackTrace
      )
  }
  final class FailedPreconditionGeohashOps private[FailedPreconditionGeohashOps] (
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

  def epsilonDefault: Double =
    ViaEntity.epsilonDefault

  def isWithinEpsilonDefault: (Double, Double) => Boolean =
    (double1, double2) =>
      Math.abs(double1 - double2) < Math.abs(epsilonDefault)

  private val (longitudeMinimum, longitudeMaximum) =
    (Longitude.minimum.toDegrees, Longitude.maximum.toDegrees)

  private val (latitudeMinimum, latitudeMaximum) =
    (Latitude.minimum.toDegrees, Latitude.maximum.toDegrees)

  private val encodeSeedArray =
    List.fill(Geohash.maxLength + 1 / 2)(List(16, 4, 1, 8, 2, 0)).flatten.map(_.toByte).toArray //0 is a filler element

  val encodeGeohashIdentity: Geohash =
    Geohash("0")

  def encode(
      coordinate: Coordinate
    , optionLength: Option[Int] = None
    , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
  ): Geohash =
    tryEncode(coordinate, optionLength, isWithinEpsilon).get

  def tryEncode(
      coordinate: Coordinate
    , optionLength: Option[Int] = None
    , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
  ): Try[Geohash] = {
    def bitify(double: Double, rangeLower: Double, rangeUpper: Double, sizeInTriplets: Int, isOffsetByOneTriplet: Boolean): Try[Array[Byte]] = {
      def copySeedArray(size: Int, offset: Int): Array[Byte] = {
        val array: Array[Byte] =
          Array.ofDim[Byte](size)
        encodeSeedArray.drop(offset).copyToArray(array)
        array
      }
      if (sizeInTriplets <= Geohash.maxLength) {
        val bitSize =
          sizeInTriplets * 3
        val array =
          copySeedArray(bitSize, if (isOffsetByOneTriplet) 3 else 0)
        @tailrec
        def recursive(index: Int, rangeLowerNew: Double, rangeUpperNew: Double): Unit =
          if (!(index < bitSize))
            ()
          else
            if (array(index) == 0)
              //skip filler element
              recursive(index + 1, rangeLowerNew, rangeUpperNew)
            else {
              val rangeMiddle =
                (rangeLowerNew + rangeUpperNew) / 2.0d
              if (double > rangeMiddle)
                //bit value already present
                recursive(index + 1, rangeMiddle, rangeUpperNew)
              else {
                //clear bit value
                array(index) = 0
                recursive(index + 1, rangeLowerNew, rangeMiddle)
              }
            }
        recursive(0, rangeLower, rangeUpper)
        Success(array)
      }
      else
        Failure(FailedPreconditionGeohashOps(s"sizeInTriplets [$sizeInTriplets] must be less than or equal to Geohaseh.maxLength [${Geohash.maxLength}]"))
    }

    def convert(longitudeBits: Array[Byte], latitudeBits: Array[Byte]): String = {
      require(longitudeBits.length == latitudeBits.length, s"longitudeBits.length [${longitudeBits.length}] must be equal to latitudeBits.length [${latitudeBits.length}]")
      @tailrec
      def recursive(index: Int, accumulator: String): String = {
        if (!(index < longitudeBits.length))
          accumulator
        else
          recursive(index + 3, accumulator + Geohash.validCharacters.charAt(longitudeBits.slice(index, index + 3).sum + latitudeBits.slice(index, index + 3).sum))
      }
      recursive(0, "")
    }

    //iterate to minimum number of characters needed to accurately (within epsilon) represent the coordinate
    //if coordinate happens to be the precise value of an existing larger geohash, the algorithm will stop at that point
    //possible improvement is to create a cache of recent value lookups to short circuit common value pathways
    def encodeConvergeToEpsilon(coordinate: Coordinate): Try[Geohash] = {
      val coordinateLongitude =
        coordinate.longitude.angle.toDegrees
      val coordinateLatitude =
        coordinate.latitude.angle.toDegrees
      @tailrec
      def recursive(length: Int, accumulator: Geohash): Try[Geohash] =
        if (!(length < Geohash.maxLength)) //using maxLength, as opposed to measuring distance to nearby geohash, to reduce the calls to heavy distance calculation
          Success(accumulator) //hit maximum length, so return last successfully calculated value
        else {
          val tryGeohash =
            tryEncode(coordinate, Some(length + 1))
          tryGeohash match {
            case Failure(_) =>
              tryGeohash
            case Success(geohash) =>
              if ( isWithinEpsilon(coordinateLongitude, geohash.center.longitude.angle.toDegrees)
                || isWithinEpsilon(coordinateLatitude, geohash.center.latitude.angle.toDegrees)
              )
                Success(geohash)
              else
                recursive(length + 1, geohash)
          }
        }
      recursive(0, encodeGeohashIdentity)
    }

    optionLength match {
      case Some(length) =>
        if (length > 0)
          for {
            longitudeBits <-
              bitify(coordinate.longitude.angle.toDegrees, longitudeMinimum, longitudeMaximum, length, isOffsetByOneTriplet = false)
            latitudeBits <-
              bitify(coordinate.latitude.angle.toDegrees, latitudeMinimum, latitudeMaximum, length, isOffsetByOneTriplet = true)
          } yield Geohash(convert(longitudeBits, latitudeBits))
        else
          Failure(FailedPreconditionGeohashOps(s"when optionLength isDefined, the value [$length] must be greater than 0"))
      case None =>
        encodeConvergeToEpsilon(coordinate)
    }
  }

  private val rowsUpperLowerEvenOdd =
    (
        (Geohash.rowsEven.head, Geohash.rowsOdd.head)
      , (Geohash.rowsEven.last, Geohash.rowsOdd.last)
    )

  private val (adjacentsByCharEven, adjacentsByCharOdd): (Map[Char, Map[Direction.Member, Char]], Map[Char, Map[Direction.Member, Char]]) = {
    val edgeChar =
      '+'
    def edgeRows(rows: List[String]) = {
      val edgeHorizontal =
        List(List.fill(rows.head.length)(edgeChar).mkString)
      (edgeHorizontal ::: rows ::: edgeHorizontal).map(row => edgeChar + row + edgeChar)
    }
    def generateAdjacents(rows: List[String]): Map[Char, Map[Direction.Member, Char]] = {
      val offsets: List[(Direction.Member, Int)] = {
        val rowWidth =
          rows.head.length
        Direction.members.zip(List(-rowWidth, -rowWidth + 1, 1, rowWidth + 1, rowWidth, rowWidth - 1, -1, -rowWidth - 1))
      }
      val array: Array[Char] =
        rows.flatten.toArray
      ( for {
          rowIndex <- 1 until rows.size - 1
          rowWidth = rows.head.length
          columnIndex <- 1 until rowWidth - 1
          index = (rowWidth * rowIndex) + columnIndex
          char = array(index)
          map =
            offsets
              .collect {
                case (direction, offset) if array(index + offset) != edgeChar =>
                  (direction, array(index + offset))
              }.toMap
        } yield (char, map)
      ).toMap
    }
    (generateAdjacents(edgeRows(Geohash.rowsEven)), generateAdjacents(edgeRows(Geohash.rowsOdd)))
  }

  private val (directionSetUpper, directionSetLower): (Set[Direction.Member], Set[Direction.Member]) =
    (Set(Direction.NORTH_WEST, Direction.NORTH, Direction.NORTH_EAST), Set(Direction.SOUTH_WEST, Direction.SOUTH, Direction.SOUTH_EAST))

  def generateAdjacents(geohash: Geohash): (Map[Direction.Member, Geohash], Boolean) = {
    def isUndefined(value: String): Option[Boolean] = {
      def isUndefined2(evenAndOdd: (String, String)): Boolean = {
        @tailrec
        def recursive(valueRemaining: String, isEven: Boolean): Boolean =
          if (valueRemaining.isEmpty)
            true
          else
            if (!(if (isEven) evenAndOdd._1 else evenAndOdd._2).contains(valueRemaining.head))
              false
            else
              recursive(valueRemaining.tail, !isEven)
        recursive(value, isEven = true)
      }
      if (isUndefined2(rowsUpperLowerEvenOdd._1))
        Some(true)
      else
        if (isUndefined2(rowsUpperLowerEvenOdd._2))
          Some(false)
        else
          None
    }

    def resolveLongitudeWrapAround(longitudeCenter: Longitude, longitudeOffset: Angle): Longitude = {
      val temp =
        longitudeCenter.angle + longitudeOffset
      if (temp < Longitude.minimum)
        Longitude(Longitude.maximum + (longitudeOffset / 2.0d))
      else
        if (temp > Longitude.maximum)
          Longitude(Longitude.minimum + (longitudeOffset / 2.0d))
        else
          Longitude(temp)
    }

    val parent =
      geohash.value.take(geohash.value.length - 1)
    val geohashsByDirection =
      ( if (geohash.value.length % 2 == 1)
          adjacentsByCharEven(geohash.value.last)
        else
          adjacentsByCharOdd(geohash.value.last)
      ).map(directionAndChar => (directionAndChar._1, Geohash(parent + directionAndChar._2)))
    val adjacents =
      if (geohashsByDirection.keySet.size != Direction.members.size) {
        //do edge detection and filling
        val directionsToFillViaCoordinate = {
          val temp =
            Direction.members.filter(direction => !geohashsByDirection.keySet.contains(direction))
          isUndefined(geohash.value) match {
            case Some(isUpper) =>
              if (isUpper)
                temp.filter(direction => !directionSetUpper.contains(direction))
              else
                temp.filter(direction => !directionSetLower.contains(direction))
            case None =>
              temp
          }
        }
        val derivedFromCoordinates: Map[Direction.Member, Geohash] = {
          val longitudeOffset =
            (geohash.center.longitude.angle - geohashsByDirection.getOrElse(Direction.WEST, geohashsByDirection(Direction.EAST)).center.longitude.angle).abs
          val latitudeOffset =
            (geohash.center.latitude.angle - geohashsByDirection.getOrElse(Direction.NORTH, geohashsByDirection(Direction.SOUTH)).center.latitude.angle).abs
          ( for {
              direction <- directionsToFillViaCoordinate
              geohash2 =
                encode(
                    direction match {
                      case Direction.NORTH =>
                        Coordinate(geohash.center.longitude, Latitude(geohash.center.latitude.angle + latitudeOffset))
                      case Direction.NORTH_EAST =>
                        Coordinate(resolveLongitudeWrapAround(geohash.center.longitude, longitudeOffset), Latitude(geohash.center.latitude.angle + latitudeOffset))
                      case Direction.EAST =>
                        Coordinate(resolveLongitudeWrapAround(geohash.center.longitude, longitudeOffset), geohash.center.latitude)
                      case Direction.SOUTH_EAST =>
                        Coordinate(resolveLongitudeWrapAround(geohash.center.longitude, longitudeOffset), Latitude(geohash.center.latitude.angle - latitudeOffset))
                      case Direction.SOUTH =>
                        Coordinate(geohash.center.longitude, Latitude(geohash.center.latitude.angle - latitudeOffset))
                      case Direction.SOUTH_WEST =>
                        Coordinate(resolveLongitudeWrapAround(geohash.center.longitude, -longitudeOffset), Latitude(geohash.center.latitude.angle - latitudeOffset))
                      case Direction.WEST =>
                        Coordinate(resolveLongitudeWrapAround(geohash.center.longitude, -longitudeOffset), geohash.center.latitude)
                      case Direction.NORTH_WEST =>
                        Coordinate(resolveLongitudeWrapAround(geohash.center.longitude, -longitudeOffset), Latitude(geohash.center.latitude.angle + latitudeOffset))
                    }
                )
            } yield (direction, geohash2)
          ).toMap
        }
        geohashsByDirection ++ derivedFromCoordinates
      }
      else
        geohashsByDirection
    (adjacents, adjacents.keySet.size == Direction.members.size)
  }

  implicit class RichGeohash(val geohash: Geohash) {
    val boundsLowerRight: Coordinate =
      Coordinate(geohash.boundsUpperRight.longitude, geohash.boundsLowerLeft.latitude)

    val boundsUpperLeft: Coordinate =
      Coordinate(geohash.boundsLowerLeft.longitude, geohash.boundsUpperRight.latitude)

    val sideLower: (Coordinate, Coordinate) =
      (geohash.boundsLowerLeft, boundsLowerRight)

    val sideUpper: (Coordinate, Coordinate) =
      (boundsUpperLeft, geohash.boundsUpperRight)

    val sideLeft: (Coordinate, Coordinate) =
      (geohash.boundsLowerLeft, boundsUpperLeft)

    val sideRight: (Coordinate, Coordinate) =
      (boundsLowerRight, geohash.boundsUpperRight)

    val centerHorizontal: (Coordinate, Coordinate) =
      (Coordinate(geohash.boundsLowerLeft.longitude, geohash.center.latitude), Coordinate(geohash.boundsUpperRight.longitude, geohash.center.latitude))

    val centerVertical: (Coordinate, Coordinate) =
      (Coordinate(geohash.center.longitude, geohash.boundsLowerLeft.latitude), Coordinate(geohash.center.longitude, geohash.boundsUpperRight.latitude))

    val optionLengthDefault =
      Some(geohash.value.length)

    def encode(
        coordinate: Coordinate
      , optionLength: Option[Int] = optionLengthDefault
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): Geohash =
      tryEncode(coordinate, optionLength, isWithinEpsilon).get

    def tryEncode(
        coordinate: Coordinate
      , optionLength: Option[Int] = optionLengthDefault
      , isWithinEpsilon: (Double, Double) => Boolean = isWithinEpsilonDefault
    ): Try[Geohash] =
      GeohashOps.tryEncode(coordinate, optionLength, isWithinEpsilon)

    val (adjacents, isAdjacentsComplete) =
      generateAdjacents(geohash)
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
