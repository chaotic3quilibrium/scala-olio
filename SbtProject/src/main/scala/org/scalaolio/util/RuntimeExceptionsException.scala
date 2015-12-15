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
