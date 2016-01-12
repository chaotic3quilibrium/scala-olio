/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.gis.spatial.surface2d.entity              **
**   Name:      Latitude.scala                                          **
**                                                                      **
** Description:                                                         **
**  Simple "Scala Case Class Pattern Instance" wrapper for              **
**  squants.Angle to explicitly limit it's range -90.0d until 90.0.d    **
**  degrees                                                             **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.gis.spatial.surface2d.entity

import scala.util.{Failure, Success, Try}

import org.scalaolio.util.FailedPreconditionsException
import org.scalaolio.util.FailedPreconditionsException.{FailedPrecondition, FailedPreconditionObject}
import squants._
import squants.space.Degrees

object Latitude extends ((Angle) => Latitude) {
  object FailedPreconditionMustBeWithinValidRange extends FailedPreconditionObject[FailedPreconditionMustBeWithinValidRange] {
    def apply(
        optionMessage: Option[String] = None
      , optionCause: Option[Throwable] = None
      , isEnableSuppression: Boolean = false
      , isWritableStackTrace: Boolean = false
    ): FailedPreconditionMustBeWithinValidRange =
      new FailedPreconditionMustBeWithinValidRange(
          optionMessage
        , optionCause
        , isEnableSuppression
        , isWritableStackTrace
      )
  }
  final class FailedPreconditionMustBeWithinValidRange private[FailedPreconditionMustBeWithinValidRange] (
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

  def apply(angle: Angle): Latitude =
    validatePreconditions(angle) match {
      case Some(failedPreconditionsException) =>
        throw failedPreconditionsException
      case None =>
        create(angle)
    }

  def tryApply(angle: Angle): Try[Latitude] =
    validatePreconditions(angle) match {
      case Some(failedPreconditionsException) =>
        Failure(failedPreconditionsException)
      case None =>
        Success(create(angle))
    }

  def tryApplyFactory(angle: Angle): Try[() => Latitude] =
    validatePreconditions(angle) match {
      case Some(failedPreconditionsException) =>
        Failure(failedPreconditionsException)
      case None =>
        Success(
          new (() => Latitude) {
            def apply(): Latitude =
              create(angle)
          }
        )
    }

  def validatePreconditions(angle: Angle): Option[FailedPreconditionsException] =
    FailedPreconditionsException.tryApply(clientValidatePreconditions(angle)).toOption

  val (minimum, maximum): (Angle, Angle) =
    (Degrees(-90.0d), Degrees(90.0d))

  def clientValidatePreconditions(angle: Angle): List[FailedPrecondition] =
    List(
      if (!((minimum <= angle) && (angle < maximum)))
        Some(FailedPreconditionMustBeWithinValidRange(s"angle.toDegrees [${angle.toDegrees}] must be greater than or equal to minimum.toDegrees [${minimum.toDegrees}] and less than maximum [${maximum.toDegrees}]"))
      else
        None
    ).flatten

  private def create(angle: Angle): Latitude =
    new Latitude(angle) {
      private def readResolve(): Object =
        Latitude(angle)

      def copy(angleNew: Angle = angle): Latitude =
        Latitude(angleNew)

      def tryCopy(angleNew: Angle = angle): Try[Latitude] =
        Latitude.tryApply(angleNew)

      def tryCopyFactory(angleNew: Angle = angle): Try[() => Latitude] =
        Latitude.tryApplyFactory(angleNew)
    }
}
abstract case class Latitude private[Latitude] (angle: Angle) {
  def copy(angleNew: Angle = angle): Latitude
  def tryCopy(angleNew: Angle = angle): Try[Latitude]
  def tryCopyFactory(angleNew: Angle = angle): Try[() => Latitude]
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
