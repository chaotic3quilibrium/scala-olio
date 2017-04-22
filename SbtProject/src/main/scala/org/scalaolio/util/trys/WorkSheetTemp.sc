import scala.util.{Failure, Try}

import org.scalaolio.util.trys._
import org.scalaolio.util.trys.template._

def evaluateTheRiskyThing: Int =
  //3/0
  throw new Exception("uhoh-Exception")
  //throw new Error("uhoh-Exception")
  //throw new Throwable("uhoh-Throwable")

//val tryTemp1 =
//  Try(evaluateTheRiskyThing)
//tryTemp1.recoverWith {
//  case runtimeException: RuntimeException =>
//    println(s"Failure: ${runtimeException.getMessage}")
//    Failure(runtimeException) //an additional instantiation
//  case throwable: Throwable =>
//    println(s"Uncaught: ${throwable.getMessage}")
//    //Surprise!!! The rethrown throwable below gets
//    //  re-caught by this Try method and
//    //  turned into a Failure anyway! ARGH!
//    throw throwable
//}
//
//val tryTemp2 =
//  Try(evaluateTheRiskyThing) match {
//    case failure @ Failure(throwable) =>
//      throwable match {
//        case runtimeException: RuntimeException =>
//          println(s"Failure: ${runtimeException.getMessage}")
//          failure
//        case _ =>
//          println(s"Uncaught: ${throwable.getMessage}")
//          throw throwable //this rethrow actually works
//      }
//    case success @ _ =>
//      success
//  }

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

val tryBase: List[TryBase[_ <: Throwable, _]] =
  List(
      TryRuntimeException(1 + 1)//(failureTEventFunc, optionThrowableEventFunc)
    , TryRuntimeException(3/0)//(failureTEventFunc, optionThrowableEventFunc)
    , TryException(2/0)
    , TryThrowable(1/0)
    , TryThrowableNonFatal(0/0)
  )

val tryBasesFailureBasesOnly =
  tryBase.zipWithIndex.filter(_._1.isFailure)

val ints: java.io.Serializable =
  5

val optionInt: Option[java.io.Serializable] =
  Some(5)

val failureBases: List[(FailureBase[_ <: Throwable, _], Option[java.io.Serializable])] =
  tryBasesFailureBasesOnly.map {
    case (a, b) =>
      val bs: Option[java.io.Serializable] =
        Some(b) //necessary to do here as the commented out implicit conversions below never happen
      (
          a.asInstanceOf[FailureBase[_ <: Throwable, _]]
        //, None
        //, Some(b) //doesn't compile - the implicit conversion never happens
        //, Some[Serializable](b) //doesn't compile - the implicit conversion never happens
        //, Some(int2Integer(b)) //turning the implicit conversion explicit
        , bs
      )
  }
val failureRuntimeException =
  FailureRuntimeException(new IllegalStateException(), failureBases)