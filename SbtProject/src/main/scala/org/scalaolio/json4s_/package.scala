/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.json4s_                                   **
**   Name:      package.scala                                           **
**                                                                      **
** Description:                                                         **
**  Augments json4s with simple extensions                              **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio

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

    def has(path: String = ""): Option[JValue] =
      has(List(path).filter(_.nonEmpty))

    def has(path: List[String]): Option[JValue] =
      via(path) match {
        case Success(jValueResult) =>
          Some(jValueResult)
        case Failure(_) =>
          None
      }

    def via(path: String = ""): Try[JValue] =
      via(List(path).filter(_.nonEmpty))

    def via(path: List[String]): Try[JValue] = {
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
              "JValue at JSON path \"" + ("jValue" :: path).mkString(" \\ ") + "\"" + e.getMessage.drop("JValue ".length)
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

    def toFloat: Try[Float] =
      jValue.toTry.flatMap {
        case jString: JString =>
          Try(jString.s.toFloat)
        case jDouble: JDouble =>
          Success(jDouble.num.toFloat)
        case jDecimal: JDecimal =>
          Success(jDecimal.num.toFloat)
        case jInt: JInt =>
          Success(jInt.num.toFloat)
        case jBool: JBool =>
          Success(if (jBool.value) 1.0f else 0.0f)
        case _ =>
          Failure(new IllegalStateException("unable to convert to Float"))
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

    def toLong: Try[Long] =
      jValue.toTry.flatMap {
        case jString: JString =>
          Try(jString.s.toLong)
        case jDouble: JDouble =>
          Success(jDouble.num.toLong)
        case jDecimal: JDecimal =>
          Success(jDecimal.num.toLong)
        case jInt: JInt =>
          Success(jInt.num.toLong)
        case jBool: JBool =>
          Success(if (jBool.value) 1L else 0L)
        case _ =>
          Failure(new IllegalStateException("unable to convert to Long"))
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

    def toKeyAndValues(keyPrefix: String = "", pathSeparator: String = "."): Try[List[(String, String)]] = {
      def recursive(jValue2: JValue, keyPrefix2: String, key: String = ""): Try[List[(String, String)]] = {
        def processListOfTuple2(keyNew: String, tuple2s: List[(JValue, String)]): Try[List[(String, String)]] = {
          val tryStringAndStrings =
            tuple2s.map(tuple2 => recursive(tuple2._1, keyNew, tuple2._2))
          if (!tryStringAndStrings.exists(_.isFailure))
            Success(tryStringAndStrings.map(_.get).flatten)
          else
            tryStringAndStrings.dropWhile(_.isSuccess).head
        }
        val keyNew =
          keyPrefix2 + (if (keyPrefix2.nonEmpty) pathSeparator else "") + key
        jValue2.toJavaString match {
          case Success(value) =>
            Success(List(keyNew -> value))
          case Failure(_) =>
            jValue2 match {
              case jObject: JObject =>
                if (jObject.obj.nonEmpty)
                  processListOfTuple2(keyNew, jObject.obj.map(jField => (jField._2, jField._1)))
                else
                  Success(Nil)
              case jArray: JArray =>
                if (jArray.arr.nonEmpty)
                  processListOfTuple2(keyNew, jArray.arr.zipWithIndex.map(jValueAndInt => (jValueAndInt._1, jValueAndInt._2.toString)))
                else
                  Success(Nil)
              case JNull => Success(Nil)
              case _ =>
                Failure(new IllegalStateException(s"should NEVER get here - missed JValue type at path [$keyPrefix2]"))
            }
        }
      }
      if ((jValue.isInstanceOf[JObject] || jValue.isInstanceOf[JArray]) || keyPrefix.nonEmpty)
        recursive(jValue, keyPrefix)
      else
        Failure(new IllegalArgumentException("when jValue is not one of either JObject or JArray, keyPrefix must be nonEmpty"))
    }
  }

  implicit def convertJValueToRichJValue(jValue: JValue): RichJValue =
    RichJValue(jValue)
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
