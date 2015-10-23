package org.scalaolio.util

import scala.util.{Failure, Success, Try}

object FailedPreconditionsException {
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

  object FailedPreconditionMustBeNonEmptyList {
    def apply: FailedPreconditionMustBeNonEmptyList =
      FailedPreconditionMustBeNonEmptyList()

    def apply(message: String): FailedPreconditionMustBeNonEmptyList =
      FailedPreconditionMustBeNonEmptyList(optionMessage = Some(message))

    def apply(cause: Throwable): FailedPreconditionMustBeNonEmptyList =
      FailedPreconditionMustBeNonEmptyList(optionCause = Some(cause))

    def apply(message: String, cause: Throwable): FailedPreconditionMustBeNonEmptyList =
      FailedPreconditionMustBeNonEmptyList(optionMessage = Some(message), optionCause = Some(cause))

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
      s"failed preconditions [${failedPreconditions.size}] have occurred - ${failedPreconditions.map(_.getMessage).mkString("|")}"
    else
      s"failed precondition has occurred - ${failedPreconditions.head.getMessage}"
}
final class FailedPreconditionsException private[FailedPreconditionsException] (
  val failedPreconditions: List[FailedPreconditionsException.FailedPrecondition]
) extends RuntimeException(FailedPreconditionsException.composeMessage(failedPreconditions))
