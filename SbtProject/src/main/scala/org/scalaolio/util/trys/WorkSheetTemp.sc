import org.scalaolio.util.trys.TryRuntimeException

import scala.util.{Failure, Try}

import org.scalaolio.util.trys.template._

def evaluateTheRiskyThing: Int =
  3/0
  //throw new Exception()
  //throw new Throwable()

val tryTemp1 =
  Try(evaluateTheRiskyThing)
tryTemp1.recoverWith {
  case throwable =>
    if (new RuntimeException().getClass.isInstance(throwable))
      tryTemp1 //called the constructor only once
    else
      throw throwable //still results in being captured as Failure(Exception) as recoverWith wraps this call with it's own try which will encapsulate any NonFatal() as a Failure - completely NOT the desired behavior
}

val tryTemp2 =
  Try(evaluateTheRiskyThing).recoverWith {
    case throwable =>
      if (new RuntimeException().getClass.isInstance(throwable)) {
        println(s"Failure: ${Option(throwable.getMessage).getOrElse("<null>")}")
        Failure(throwable) //called the constructor a second time
      }
      else {
        println(s"Uncaught: ${Option(throwable.getMessage).getOrElse("<null>")}")
        throw throwable //still results in being captured as Failure(Exception) as recoverWith wraps this call with it's own try which will encapsulate any NonFatal() as a Failure - completely NOT the desired behavior
      }
  }

val tryRuntimeException1 =
  TryRuntimeException(
      evaluateTheRiskyThing
  ) (
      failureTEvent =
        failureBase =>
          println(s"Failure: ${Option(failureBase.t.getMessage).getOrElse("<null>")}")
    , optionThrowableEvent =
        Some(
          (
              (throwable: Throwable) =>
                true
            , (throwable: Throwable) =>
                println(s"Uncaught: ${Option(throwable.getMessage).getOrElse("<null>")}")
          )
        )
  )

val tryRuntimeException1a =
  TryRuntimeException(evaluateTheRiskyThing) (
      failureBase =>
        println(s"Failure: ${Option(failureBase.t.getMessage).getOrElse("<null>")}")
    , Some(
        (
            (_: Throwable) =>
              true
          , (throwable: Throwable) =>
              println(s"Uncaught: ${Option(throwable.getMessage).getOrElse("<null>")}")
        )
      )
  )

implicit val failureTEventFunc =
  (failureBase: FailureBase[RuntimeException, _]) =>
    println(s"Failure: ${Option(failureBase.t.getMessage).getOrElse("<null>")}")

implicit val optionThrowableEventFunc =
    Some(
      (
          (_: Throwable) =>
            true
        , (throwable: Throwable) =>
            println(s"Uncaught: ${Option(throwable.getMessage).getOrElse("<null>")}")
      )
    )

val tryRuntimeException2 =
  TryRuntimeException(evaluateTheRiskyThing)
