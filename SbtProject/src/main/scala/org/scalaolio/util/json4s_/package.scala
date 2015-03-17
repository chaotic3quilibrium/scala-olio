package org.scalaolio.util

import scala.annotation.tailrec
import scala.language.implicitConversions

import scala.util.{Failure, Success, Try}

import org.json4s._

import org.scalaolio.java.lang.Class_._

package object json4s_ {
  object RichJValue {
    def apply(jValue: JValue): RichJValue =
      new RichJValue(jValue)

    sealed private[RichJValue] abstract case class Impl(
        jValue: JValue
    )
  }
  final class RichJValue private [RichJValue] (
      jValue: JValue
  ) extends RichJValue.Impl(jValue) {
    val toTry: Try[JValue] =
      jValue.toOption match {
        case Some(jValueGet) =>
          Success(jValueGet)
        case None =>
          Failure(new IllegalArgumentException(s"jValue isEmpty"))
      }

    def via(path: List[String] = Nil): Try[JValue] = {
      @tailrec
      def recursive(tryJValue: Try[JValue], pathRemaining: List[String]): Try[JValue] =
        if (pathRemaining.isEmpty || tryJValue.isFailure)
          tryJValue
        else {
          val jValueGet = tryJValue.get //couldn't flatMap as it broke tail recursion
          val jValueNext: Try[JValue] =
            if (!pathRemaining.head.head.isDigit)
              (jValueGet \ pathRemaining.head).toTry
            else {
              jValueGet match {
                case jArray: JArray =>
                  Try(pathRemaining.head.toInt) match {
                    case Success(index) =>
                      val tryJArrayIndex =
                        Try(jArray(index))
                      tryJArrayIndex match {
                        case Success(_) =>
                          tryJArrayIndex
                        case Failure(e) =>
                          Failure(new IllegalStateException(s"failed to access JArray[$index]"))
                      }
                    case Failure(e) =>
                      Failure(new IllegalStateException(s"failed to convert pathRemaining.head [${pathRemaining.head}] to integer for index into JArray"))
                  }
                case _ =>
                  Failure(new IllegalStateException(s"failed to use pathRemaining.head [${pathRemaining.head}] as JValue is not an instance of JArray - ${jValueGet.getClass.getName}"))
              }
            }
          recursive(
              jValueNext match {
                case Success(_) =>
                  jValueNext
                case Failure(e) =>
                  val message =
                    path.take(path.size - (pathRemaining.size - 1)).mkString(" \\ ")
                  Failure(new IllegalStateException(s"JSON path 'jValue \\ $message' not defined - ${e.getMessage}"))
              }
            , pathRemaining.tail
          )
        }
      recursive(jValue.toTry, path)
    }

    def typed[A <: JValue](aGetClassSimpleName: String, path: List[String]): Try[A] = {
      def classSimpleNameMatch(jValue: JValue): Try[A] = {
        val jValueGetClassSimpleName = jValue.getClass.simpleName
        if (jValueGetClassSimpleName == aGetClassSimpleName)
          Success(jValue.asInstanceOf[A])
        else
          Failure(new IllegalStateException(s"JValue is not of type $aGetClassSimpleName - found $jValueGetClassSimpleName"))
      }
      jValue.via(path).flatMap(
        classSimpleNameMatch(_) match {
          case Success(a) =>
            Success(a)
          case Failure(e) =>
            val message =
              "JValue at JSON path \"" + ("jValue" :: path).mkString(" \\ ") + "\"" + e.getMessage.drop("JValue ".size)
            Failure(new IllegalStateException(message))
        }
      )
    }

    def viaJString(path: String = ""): Try[JString] =
      viaJString(List(path).filter(_.nonEmpty))

    def viaJString(path: List[String]): Try[JString] =
      typed[JString](JString.getClass.simpleName, path)

    def viaJDouble(path: String = ""): Try[JDouble] =
      viaJDouble(List(path).filter(_.nonEmpty))

    def viaJDouble(path: List[String]): Try[JDouble] =
      typed[JDouble](JDouble.getClass.simpleName, path)

    def viaJDecimal(path: String = ""): Try[JDecimal] =
      viaJDecimal(List(path).filter(_.nonEmpty))

    def viaJDecimal(path: List[String]): Try[JDecimal] =
      typed[JDecimal](JDecimal.getClass.simpleName, path)

    def viaJInt(path: String = ""): Try[JInt] =
      viaJInt(List(path).filter(_.nonEmpty))

    def viaJInt(path: List[String]): Try[JInt] =
      typed[JInt](JInt.getClass.simpleName, path)

    def viaJBool(path: String = ""): Try[JBool] =
      viaJBool(List(path).filter(_.nonEmpty))

    def viaJBool(path: List[String]): Try[JBool] =
      typed[JBool](JBool.getClass.simpleName, path)

    def viaJObject(path: String = ""): Try[JObject] =
      viaJObject(List(path).filter(_.nonEmpty))

    def viaJObject(path: List[String]): Try[JObject] =
      typed[JObject](JObject.getClass.simpleName, path)

    def viaJArray(path: String = ""): Try[JArray] =
      viaJArray(List(path).filter(_.nonEmpty))

    def viaJArray(path: List[String]): Try[JArray] =
      typed[JArray](JArray.getClass.simpleName, path)

    def toJavaString: Try[String] =
      jValue.toTry.flatMap {
        case jString: JString =>
          Success(jString.s)
        case jDouble: JDouble =>
          Success(jDouble.num.toString)
        case jDecimal: JDecimal =>
          Success(jDecimal.num.toString())
        case jInt: JInt =>
          Success(jInt.num.toString())
        case jBool: JBool =>
          Success(jBool.value.toString)
        case _ =>
          Failure(new IllegalStateException("unable to convert to String"))
      }

    def toDouble: Try[Double] =
      jValue.toTry.flatMap {
        case jString: JString =>
          Try(jString.s.toDouble)
        case jDouble: JDouble =>
          Success(jDouble.num)
        case jDecimal: JDecimal =>
          Success(jDecimal.num.toDouble)
        case jInt: JInt =>
          Success(jInt.num.toDouble)
        case jBool: JBool =>
          Success(if (jBool.value) 1.0d else 0.0d)
        case _ =>
          Failure(new IllegalStateException("unable to convert to Double"))
      }

    def toInt: Try[Int] =
      jValue.toTry.flatMap {
        case jString: JString =>
          Try(jString.s.toInt)
        case jDouble: JDouble =>
          Success(jDouble.num.toInt)
        case jDecimal: JDecimal =>
          Success(jDecimal.num.toInt)
        case jInt: JInt =>
          Success(jInt.num.toInt)
        case jBool: JBool =>
          Success(if (jBool.value) 1 else 0)
        case _ =>
          Failure(new IllegalStateException("unable to convert to Int"))
      }

    def toBoolean: Try[Boolean] =
      jValue.toTry.flatMap {
        case jString: JString =>
          Try(jString.s.toBoolean)
        case jDouble: JDouble =>
          Success(jDouble.num != 0.0d)
        case jDecimal: JDecimal =>
          Success(jDecimal.num != BigDecimal(0.0d))
        case jInt: JInt =>
          Success(jInt.num.toInt != 0)
        case jBool: JBool =>
          Success(jBool.value)
        case _ =>
          Failure(new IllegalStateException("unable to convert to Boolean"))
      }
  }

  implicit def convertJValueToRichJValue(jValue: JValue): RichJValue =
    RichJValue(jValue)
}
