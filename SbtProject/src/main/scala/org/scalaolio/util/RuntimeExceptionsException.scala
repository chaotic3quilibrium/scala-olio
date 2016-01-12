/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util                                      **
**   Name:      FailedPreconditionsException.scala                      **
**                                                                      **
** Description:                                                         **
**  Create convenience class for explicitly returning                   **
** List[RuntimeException] as a single RuntimeException (including       **
** concatenating the messages from all the Exceptions within the List)  **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util

import scala.util.{Failure, Success, Try}
import FailedPreconditionsException.{FailedPrecondition, FailedPreconditionObject}

object RuntimeExceptionsException {
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

  def apply(runtimeException: RuntimeException): RuntimeExceptionsException =
    RuntimeExceptionsException(List(runtimeException))

  def apply(runtimeExceptions: List[RuntimeException]): RuntimeExceptionsException =
    tryApply(runtimeExceptions).get

  def tryApply(runtimeException: RuntimeException): Try[RuntimeExceptionsException] =
    tryApply(List(runtimeException))

  def tryApply(runtimeExceptions: List[RuntimeException]): Try[RuntimeExceptionsException] =
    if (runtimeExceptions.nonEmpty)
      Success(new RuntimeExceptionsException(runtimeExceptions))
    else
      Failure(FailedPreconditionsException(FailedPreconditionMustBeNonEmptyList()))

  private def composeMessage(runtimeExceptions: List[RuntimeException]): String =
    if (runtimeExceptions.size > 1)
      s"runtime exceptions [${runtimeExceptions.size}] have occurred - ${runtimeExceptions.map(_.getMessage).mkString("|")}"
    else
      s"runtime exception has occurred - ${runtimeExceptions.head.getMessage}"
}
final class RuntimeExceptionsException private[RuntimeExceptionsException] (
  val runtimeExceptions: List[RuntimeException]
) extends RuntimeException(RuntimeExceptionsException.composeMessage(runtimeExceptions))
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
