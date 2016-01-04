/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.gis.spatial.surface2d.geohash             **
**   Name:      Geohash.scala                                           **
**                                                                      **
** Description:                                                         **
**  "Scala Case Class Pattern Instance" wrapper for geohash string      **
**  exposing the specific geospatial values as strongly typed entities  **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.gis.spatial.surface2d.geohash

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

import org.scalaolio.gis.spatial.surface2d.entity.{Coordinate, Latitude, Longitude}
import org.scalaolio.util.FailedPreconditionsException
import org.scalaolio.util.FailedPreconditionsException.{FailedPrecondition, FailedPreconditionObject}
import squants.space.Degrees

object Geohash {
  object FailedPreconditionGeohash extends FailedPreconditionObject[FailedPreconditionGeohash] {
    def apply(
        optionMessage: Option[String] = None
      , optionCause: Option[Throwable] = None
      , isEnableSuppression: Boolean = false
      , isWritableStackTrace: Boolean = false
    ): FailedPreconditionGeohash =
      new FailedPreconditionGeohash(
          optionMessage
        , optionCause
        , isEnableSuppression
        , isWritableStackTrace
      )
  }
  final class FailedPreconditionGeohash private[FailedPreconditionGeohash] (
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

  val validCharacters: String =
    "0123456789bcdefghjkmnpqrstuvwxyz" //missing characters: ailo

  val (rowsEven, rowsOdd) = {
    val even =
      List(
          "bcfguvyz"
        , "89destwx"
        , "2367kmqr"
        , "0145hjnp"
      )
    val odd = {
      val rowWidth =
        even.head.length
      (-rowWidth + 1 to 0).map(index => even.map(_.charAt(-index)).reverse.mkString).toList
    }
    (even, odd)
  }

  val maxLength: Int =
    19 //attempting values above 19 start resulting in failures within encodeConvergeToEpsilon - also, choosing an odd number ensures the resulting geohash's width and length are very close; i.e. square-like

  def apply(value: String): Geohash =
    validatePreconditions(value) match {
      case Some(failedPreconditionsException) =>
        throw failedPreconditionsException
      case None =>
        create(value)
    }

  def tryApply(value: String): Try[Geohash] =
    validatePreconditions(value) match {
      case Some(failedPreconditionsException) =>
        Failure(failedPreconditionsException)
      case None =>
        Success(create(value))
    }

  def tryApplyFactory(value: String): Try[() => Geohash] =
    validatePreconditions(value) match {
      case Some(failedPreconditionsException) =>
        Failure(failedPreconditionsException)
      case None =>
        Success(
          new (() => Geohash) {
            def apply(): Geohash =
              create(value)
          }
        )
    }

  def validatePreconditions(value: String): Option[FailedPreconditionsException] =
    FailedPreconditionsException.tryApply(clientValidatePreconditions(value)).toOption

  def clientValidatePreconditions(value: String): List[FailedPrecondition] =
    if (value.isEmpty)
      List(FailedPreconditionGeohash(s"value must not be isEmpty"))
    else
      List(
          if (!(value.length <= maxLength))
            Some(FailedPreconditionGeohash(s"value.length [${value.length}] must be less than or equal to maxLength [$maxLength]"))
          else
            None
        , {
            val invalidCharacters =
              value.filter(char => !validCharacters.contains(char.toLower))
            if (!invalidCharacters.isEmpty)
              Some(FailedPreconditionGeohash(s"found (invalid) characters [$invalidCharacters] which do not appear in validCharacters [$validCharacters]"))
            else
              None
          }
      ).flatten

  private val listBooleanByByte: Map[Byte, List[Boolean]] = {
    val bits = List(64, 32, 16, 8, 4, 2, 1).map(_.toByte)
    def convert(byte: Byte): List[Boolean] =
      (byte < 0) :: bits.map(bitValue => (byte & bitValue) > 0)
    (-128 until 128).map(int => (int.toByte, convert(int.toByte))).toMap
  }

  private val listBooleanX3X2ByChar: Map[Char, (List[Boolean], List[Boolean])] = {
    val listBooleanX5ByChar: Map[Char, List[Boolean]] =
      validCharacters.zipWithIndex.map(charAndIndex => (charAndIndex._1, listBooleanByByte(charAndIndex._2.toByte).drop(3))).toMap
    listBooleanX5ByChar.map(charAndListBoolean => (charAndListBoolean._1, (List(charAndListBoolean._2.head, charAndListBoolean._2(2), charAndListBoolean._2(4)), List(charAndListBoolean._2(1), charAndListBoolean._2(3)))))
  }

  private val (longitudeMinimum, longitudeMaximum) =
    (Longitude.minimum.toDegrees, Longitude.maximum.toDegrees)

  private val (latitudeMinimum, latitudeMaximum) =
    (Latitude.minimum.toDegrees, Latitude.maximum.toDegrees)

  protected def create(value: String): Geohash =
    new Geohash(value.toLowerCase) {
      private def readResolve() =
        Geohash(value)

      val (boundsLowerLeft, boundsUpperRight) = {
        def toBits: (List[Boolean], List[Boolean]) = {
          value
            .map(listBooleanX3X2ByChar(_))
            .zipWithIndex
            .foldLeft(
              (List[Boolean](), List[Boolean]())
            )(
              (x, y) =>
                if (y._2 % 2 == 0)
                  (x._1 ::: y._1._1, x._2 ::: y._1._2)
                else
                  (x._1 ::: y._1._2, x._2 ::: y._1._1)
            )
        }

        def narrow(bits: List[Boolean], rangeLower: Double, rangeUpper: Double): (Double, Double) = {
          @tailrec
          def recursive(bitsRemaining: List[Boolean], rangeLowerNew: Double, rangeUpperNew: Double): (Double, Double) =
            if (bitsRemaining.isEmpty)
              (rangeLowerNew, rangeUpperNew)
            else {
              val rangeMiddle =
                (rangeLowerNew + rangeUpperNew) / 2.0d
              if (bitsRemaining.head)
                recursive(bitsRemaining.tail, rangeMiddle, rangeUpperNew)
              else
                recursive(bitsRemaining.tail, rangeLowerNew, rangeMiddle)
            }
          recursive(bits, rangeLower, rangeUpper)
        }
        val (listLongitudeBits, listLatitudeBits) =
          toBits
        val (longitudeMin, longitudeMax) =
          narrow(listLongitudeBits, longitudeMinimum, longitudeMaximum)
        val (latitudeMin, latitudeMax) =
          narrow(listLatitudeBits, latitudeMinimum, latitudeMaximum)
        (Coordinate(Longitude(Degrees(longitudeMin)), Latitude(Degrees(latitudeMin))), Coordinate(Longitude(Degrees(longitudeMax)), Latitude(Degrees(latitudeMax))))
      }

      val center: Coordinate =
        Coordinate(
            Longitude((boundsLowerLeft.longitude.angle + boundsUpperRight.longitude.angle) / 2.0d)
          , Latitude((boundsLowerLeft.latitude.angle + boundsUpperRight.latitude.angle) / 2.0d)
        )

      def copy(valueNew: String = value): Geohash =
        Geohash(valueNew)

      def tryCopy(valueNew: String = value): Try[Geohash] =
        Geohash.tryApply(valueNew)

      def tryCopyFactory(valueNew: String = value): Try[() => Geohash] =
        Geohash.tryApplyFactory(valueNew)
    }
}
abstract case class Geohash private[Geohash] (value: String) {
  def boundsLowerLeft: Coordinate
  def boundsUpperRight: Coordinate
  def center: Coordinate

  def copy(valueNew: String = value): Geohash
  def tryCopy(valueNew: String = value): Try[Geohash]
  def tryCopyFactory(valueNew: String = value): Try[() => Geohash]
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
