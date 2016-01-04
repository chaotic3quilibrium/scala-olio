/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.gis.spatial.surface2d.entity              **
**   Name:      Distance.scala                                          **
**                                                                      **
** Description:                                                         **
**  Simple "Scala Case Class Pattern Instance" wrapper for              **
**  squants.Length to explicitly limit it's range equal to or greater   **
**  than 0.0d                                                           **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.gis.spatial.surface2d.entity

import scala.util.{Failure, Success, Try}

import org.scalaolio.util.FailedPreconditionsException
import org.scalaolio.util.FailedPreconditionsException.{FailedPrecondition, FailedPreconditionObject}
import squants._

object Distance extends ((Length) => Distance) {
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

  def apply(length: Length): Distance =
    validatePreconditions(length) match {
      case Some(failedPreconditionsException) =>
        throw failedPreconditionsException
      case None =>
        create(length)
    }

  def tryApply(length: Length): Try[Distance] =
    validatePreconditions(length) match {
      case Some(failedPreconditionsException) =>
        Failure(failedPreconditionsException)
      case None =>
        Success(create(length))
    }

  def tryApplyFactory(length: Length): Try[() => Distance] =
    validatePreconditions(length) match {
      case Some(failedPreconditionsException) =>
        Failure(failedPreconditionsException)
      case None =>
        Success(
          new (() => Distance) {
            def apply(): Distance =
              create(length)
          }
        )
    }

  def validatePreconditions(length: Length): Option[FailedPreconditionsException] =
    FailedPreconditionsException.tryApply(clientValidatePreconditions(length)).toOption

  def clientValidatePreconditions(length: Length): List[FailedPrecondition] =
    List(
      if (!(Meters(0.0) <= length))
        Some(FailedPreconditionMustBeWithinValidRange(s"length.toMeters [${length.toMeters}] must be greater than or equal to 0.0d"))
      else
        None
    ).flatten

  private def create(length: Length): Distance =
    new Distance(length) {
      private def readResolve(): Object =
        Distance(length)

      def copy(lengthNew: Length = length): Distance =
        Distance(lengthNew)

      def tryCopy(lengthNew: Length = length): Try[Distance] =
        Distance.tryApply(lengthNew)

      def tryCopyFactory(lengthNew: Length = length): Try[() => Distance] =
        Distance.tryApplyFactory(lengthNew)
    }
}
abstract case class Distance private[Distance] (length: Length) {
  def copy(lengthNew: Length = length): Distance
  def tryCopy(lengthNew: Length = length): Try[Distance]
  def tryCopyFactory(lengthNew: Length = length): Try[() => Distance]
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
