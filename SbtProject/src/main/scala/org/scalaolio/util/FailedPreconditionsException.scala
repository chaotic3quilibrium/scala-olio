/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util                                      **
**   Name:      FailedPreconditionsException.scala                      **
**                                                                      **
** Description:                                                         **
**  Create convenience class for explicitly creating                    **
**  FailedPreconditionException descendants to ease pattern matches on  **
**  specific failures                                                   **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util

import scala.util.{Failure, Success, Try}

object FailedPreconditionsException {
  trait FailedPreconditionObject[F <: FailedPrecondition] {
    def apply: F =
      apply()

    def apply(message: String): F =
      apply(optionMessage = Some(message))

    def apply(cause: Throwable): F =
      apply(optionCause = Some(cause))

    def apply(message: String, cause: Throwable): F =
      apply(optionMessage = Some(message), optionCause = Some(cause))

    def apply(
        optionMessage: Option[String] = None
      , optionCause: Option[Throwable] = None
      , isEnableSuppression: Boolean = false
      , isWritableStackTrace: Boolean = false
    ): F
  }
  abstract class FailedPrecondition (
      val optionMessage: Option[String]
    , val optionCause: Option[Throwable]
    , val isEnableSuppression: Boolean
    , val isWritableStackTrace: Boolean
  ) extends RuntimeException(
    optionMessage match {
      case Some(string) => string
      case None => null
    },
    optionCause match {
      case Some(throwable) => throwable
      case None => null
    },
    isEnableSuppression,
    isWritableStackTrace
  )

  object FailedPreconditionMustBeNonEmptyList extends FailedPreconditionObject[FailedPreconditionMustBeNonEmptyList] {
    def apply(
        optionMessage: Option[String] = None
      , optionCause: Option[Throwable] = None
      , isEnableSuppression: Boolean = false
      , isWritableStackTrace: Boolean = false
    ): FailedPreconditionMustBeNonEmptyList =
      new FailedPreconditionMustBeNonEmptyList(
          optionMessage
        , optionCause
        , isEnableSuppression
        , isWritableStackTrace
      )
  }
  final class FailedPreconditionMustBeNonEmptyList private[FailedPreconditionMustBeNonEmptyList] (
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

  def apply(failedPrecondition: FailedPrecondition): FailedPreconditionsException =
    FailedPreconditionsException(List(failedPrecondition))

  def apply(failedPreconditions: List[FailedPrecondition]): FailedPreconditionsException =
    tryApply(failedPreconditions).get

  def tryApply(failedPrecondition: FailedPrecondition): Try[FailedPreconditionsException] =
    tryApply(List(failedPrecondition))

  def tryApply(failedPreconditions: List[FailedPrecondition]): Try[FailedPreconditionsException] =
    if (failedPreconditions.nonEmpty)
      Success(new FailedPreconditionsException(failedPreconditions))
    else
      Failure(FailedPreconditionMustBeNonEmptyList())

  private def composeMessage(failedPreconditions: List[FailedPrecondition]): String =
    if (failedPreconditions.size > 1)
      s"failed precondition exceptions [${failedPreconditions.size}] have occurred - ${failedPreconditions.map(_.optionMessage.getOrElse("")).mkString("|")}"
    else
      s"failed precondition exception has occurred - ${failedPreconditions.head.optionMessage.getOrElse("")}"
}
final class FailedPreconditionsException private[FailedPreconditionsException] (
  val failedPreconditions: List[FailedPreconditionsException.FailedPrecondition]
) extends RuntimeException(FailedPreconditionsException.composeMessage(failedPreconditions))
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
