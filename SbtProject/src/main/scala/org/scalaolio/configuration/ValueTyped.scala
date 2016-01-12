/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.configuration                             **
**   Name:      ValueTyped.scala                                        **
**                                                                      **
** Description:                                                         **
**  Enables typed (stronger than String) access to a particular         **
**  configuration value                                                 **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.configuration

import scala.util.{Failure, Success, Try}

import org.joda.time.{DateTime, DateTimeZone}

import org.scalaolio.java.lang.String_._

trait ValueTyped {
  final val DEFAULT_EMPTY_VALUE_STRING_MEANS_BOOLEAN: Boolean = false
  final val DEFAULT_EMPTY_VALUE_STRING_MEANS_BYTE: Byte = 0
  final val DEFAULT_EMPTY_VALUE_STRING_MEANS_SHORT: Short = 0
  final val DEFAULT_EMPTY_VALUE_STRING_MEANS_CHAR: Char = 0
  final val DEFAULT_EMPTY_VALUE_STRING_MEANS_INT: Int = 0
  final val DEFAULT_EMPTY_VALUE_STRING_MEANS_LONG: Long = 0L
  final val DEFAULT_EMPTY_VALUE_STRING_MEANS_FLOAT: Float = 0.0f
  final val DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE: Double = 0.0d
  final val DEFAULT_EMPTY_VALUE_STRING_MEANS_STRING: String = ""
  final val DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_INT: BigInt = BigInt(DEFAULT_EMPTY_VALUE_STRING_MEANS_INT)
  final val DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_DECIMAL: BigDecimal = BigDecimal(DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE)
  final val DEFAULT_VALUES_BOOLEAN_TRUE_LOWER_CASE: Set[String] = Set("Y", "Yes", "T", "True", "H", "High", "On").map(_.toLowerCase)
  final val DEFAULT_LIST_STRING_SEPARATOR = ","
  final val DEFAULT_SET_STRING_SEPARATOR = DEFAULT_LIST_STRING_SEPARATOR
  final val DEFAULT_MAP_STRING_STRING_SEPARATOR = DEFAULT_LIST_STRING_SEPARATOR
  final val DEFAULT_MAP_STRING_STRING_KEY_VALUE_SEPARATOR = "->"

  def fromStringToTyped[A](
      key: String
    , parse: String => Try[A]
    , emptyValueStringHasMeaning: Boolean
    , emptyValueStringMeans: () => A  //only relevant if emptyValueStringHasMeaning is true
  ): Try[A]

  //Simple - primitives
  def boolean(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: Boolean = DEFAULT_EMPTY_VALUE_STRING_MEANS_BOOLEAN //only relevant if emptyValueStringHasMeaning is true
    , valuesBooleanTrueLowerCase: Set[String] = DEFAULT_VALUES_BOOLEAN_TRUE_LOWER_CASE
  ): Try[Boolean] =
    fromStringToTyped(key, value => Try(valuesBooleanTrueLowerCase.contains(value.toLowerCase)), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  def byte(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: Byte = DEFAULT_EMPTY_VALUE_STRING_MEANS_BYTE //only relevant if emptyValueStringHasMeaning is true
  ): Try[Byte] =
    fromStringToTyped(key, value => Try(value.toByte), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  def short(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: Short = DEFAULT_EMPTY_VALUE_STRING_MEANS_SHORT //only relevant if emptyValueStringHasMeaning is true
  ): Try[Short] =
    fromStringToTyped(key, value => Try(value.toShort), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  def char(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: Char = DEFAULT_EMPTY_VALUE_STRING_MEANS_CHAR //only relevant if emptyValueStringHasMeaning is true
  ): Try[Char] =
    fromStringToTyped(key, value => Try(value.head), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  def int(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: Int = DEFAULT_EMPTY_VALUE_STRING_MEANS_INT //only relevant if emptyValueStringHasMeaning is true
  ): Try[Int] =
    fromStringToTyped(key, value => Try(value.toInt), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  def long(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: Long = DEFAULT_EMPTY_VALUE_STRING_MEANS_LONG //only relevant if emptyValueStringHasMeaning is true
  ): Try[Long] =
    fromStringToTyped(key, value => Try(value.toLong), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  def float(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: Float = DEFAULT_EMPTY_VALUE_STRING_MEANS_FLOAT //only relevant if emptyValueStringHasMeaning is true
  ): Try[Float] =
    fromStringToTyped(key, value => Try(value.toFloat), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  def double(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: Double = DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE //only relevant if emptyValueStringHasMeaning is true
  ): Try[Double] =
    fromStringToTyped(key, value => Try(value.toDouble), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  //simple - Classes
  def string(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: String = DEFAULT_EMPTY_VALUE_STRING_MEANS_STRING //only relevant if emptyValueStringHasMeaning is true
  ): Try[String] =
    fromStringToTyped(key, value => Success(value), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  def bigInt(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: BigInt = DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_INT //only relevant if emptyValueStringHasMeaning is true
  ): Try[BigInt] =
    fromStringToTyped(key, value => Try(BigInt(value)), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  def bigDecimal(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: BigDecimal = DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_DECIMAL //only relevant if emptyValueStringHasMeaning is true
  ): Try[BigDecimal] =
    fromStringToTyped(key, value => Try(BigDecimal(value)), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  def dateTime(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: DateTime = new DateTime //only relevant if emptyValueStringHasMeaning is true
  ): Try[DateTime] =
    fromStringToTyped(key, value => Try(new DateTime(value)), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  def utcDateTime(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: DateTime = new DateTime(DateTimeZone.UTC) //only relevant if emptyValueStringHasMeaning is true
  ): Try[DateTime] =
    fromStringToTyped(key, value => Try(new DateTime(value, DateTimeZone.UTC)), emptyValueStringHasMeaning, () => emptyValueStringMeans)

  //complex - classes
  def listString(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: List[String] = Nil //only relevant if emptyValueStringHasMeaning is true
    , defaultSeparator: String = DEFAULT_LIST_STRING_SEPARATOR
    , isDefaultSeparatorRegex: Boolean = false
  ): Try[List[String]] = {
    def f(value: String): Try[List[String]] =
      Try(
        if (isDefaultSeparatorRegex)
          value.split(defaultSeparator).toList
        else
          value.splitLiterally(defaultSeparator)
      )
    fromStringToTyped(key, f, emptyValueStringHasMeaning, () => emptyValueStringMeans)
  }

  def list[A](
      key: String
    , parser: String => Try[A]
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: List[A] = Nil //only relevant if emptyValueStringHasMeaning is true
    , defaultSeparator: String = DEFAULT_LIST_STRING_SEPARATOR
    , isDefaultSeparatorRegex: Boolean = false
  ): Try[List[A]] =
    listString(key, emptyValueStringHasMeaning, Nil, defaultSeparator, isDefaultSeparatorRegex).flatMap(
      values => {
        val tryValueAndAs =
          values.map(value => (value, parser(value)))
        if (!tryValueAndAs.exists(_._2.isFailure))
          Success(tryValueAndAs.map(_._2.get))
        else {
          val failureAs =
            tryValueAndAs.filter(_._2.isFailure).map(tuple2 => (tuple2._1, tuple2._2.asInstanceOf[Failure[_]]))
          val plural =
            if (failureAs.size > 1) "s" else ""
          val failureAsToString =
            failureAs.map(tuple2 => s"${tuple2._1}=>${tuple2._2.exception.getMessage}").mkString("|&&|")
          Failure(new IllegalStateException(s"parser failed on value$plural - $failureAsToString"))
        }
      }
    )

  def setString(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: Set[String] = Set() //only relevant if emptyValueStringHasMeaning is true
    , defaultSeparator: String = DEFAULT_SET_STRING_SEPARATOR
    , isDefaultSeparatorRegex: Boolean = false
  ): Try[Set[String]] =
    listString(key, emptyValueStringHasMeaning, Nil, defaultSeparator, isDefaultSeparatorRegex).map(_.toSet)

  def set[A](
      key: String
    , parser: String => Try[A]
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: Set[A] = Set() //only relevant if emptyValueStringHasMeaning is true
    , defaultSeparator: String = DEFAULT_SET_STRING_SEPARATOR
    , isDefaultSeparatorRegex: Boolean = false
  ): Try[Set[A]] =
    list[A](key, parser, emptyValueStringHasMeaning, Nil, defaultSeparator, isDefaultSeparatorRegex).map(_.toSet)

  def mapStringString(
      key: String
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: Map[String, String] = Map() //only relevant if emptyValueStringHasMeaning is true
    , defaultSeparator: String = DEFAULT_MAP_STRING_STRING_SEPARATOR
    , isDefaultSeparatorRegex: Boolean = false
    , defaultKeyValueSeparator: String = DEFAULT_MAP_STRING_STRING_KEY_VALUE_SEPARATOR
    , isDefaultKeyValueSeparatorRegex: Boolean = false
    , emptyMapValueHasMeaning: Boolean = false
  ): Try[Map[String, String]] =
    listString(key, emptyValueStringHasMeaning, Nil, defaultSeparator, isDefaultSeparatorRegex).flatMap(
      listStringKeyAndValue => {
        val listTryUnparsedAndTryParsed =
          listStringKeyAndValue.map(
            keyAndValue => {
              val parsed =
                if (isDefaultKeyValueSeparatorRegex)
                  keyAndValue.split(defaultKeyValueSeparator).toList
                else
                  keyAndValue.splitLiterally(defaultKeyValueSeparator)
              val tryTuple2StringString: Try[(String, String)] =
                if (parsed.size == 2) {
                  val (key, value) = (parsed.head, parsed(1))
                  if (key.nonEmpty)
                    if (value.nonEmpty || emptyMapValueHasMeaning)
                      Success((key, value))
                    else
                      Failure(new IllegalStateException(s"for keyAndValue [$keyAndValue] using defaultKeyValueSeparator [$defaultKeyValueSeparator], when emptyMapValueHasMeaning is false the parsed value must be nonEmpty"))
                  else
                    Failure(new IllegalStateException(s"for keyAndValue [$keyAndValue] using defaultKeyValueSeparator [$defaultKeyValueSeparator], the parsed key must be nonEmpty"))
                }
                else
                  Failure(new IllegalStateException(s"for keyAndValue [$keyAndValue] using defaultKeyValueSeparator [$defaultKeyValueSeparator], parsed.length [${parsed.length}] was not equal to 2"))
              (keyAndValue, tryTuple2StringString)
            }
          )
        if (!listTryUnparsedAndTryParsed.exists(_._2.isFailure))
          Success(listTryUnparsedAndTryParsed.map(_._2.get).toMap)
        else {
          val listTryUnparsedAndFailureParsed =
            listTryUnparsedAndTryParsed.filter(_._2.isFailure).map(tuple2 => (tuple2._1, tuple2._2.asInstanceOf[Failure[_]]))
          val plural =
            if (listTryUnparsedAndFailureParsed.size > 1) "s" else ""
          val failureAsToString =
            listTryUnparsedAndFailureParsed.map(tuple2 => s"${tuple2._1}=>${tuple2._2.exception.getMessage}").mkString("|&&|")
          Failure(new IllegalStateException(s"parser failed on key/value pair$plural - $failureAsToString"))
        }
      }
    )

  def map[A, B](
      key: String
    , parserA: String => Try[A]
    , parserB: String => Try[B]
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: Map[A, B] = Map() //only relevant if emptyValueStringHasMeaning is true
    , defaultSeparator: String = DEFAULT_MAP_STRING_STRING_SEPARATOR
    , isDefaultSeparatorRegex: Boolean = false
    , defaultKeyValueSeparator: String = DEFAULT_MAP_STRING_STRING_KEY_VALUE_SEPARATOR
    , isDefaultKeyValueSeparatorRegex: Boolean = false
    , emptyMapValueHasMeaning: Boolean = false
  ): Try[Map[A, B]] =
    mapStringString(key, emptyValueStringHasMeaning, Map(), defaultSeparator, isDefaultSeparatorRegex, defaultKeyValueSeparator, isDefaultKeyValueSeparatorRegex, emptyMapValueHasMeaning).flatMap(
      stringValueByStringKey => {
        val stringKeyAndTryAAndStringValueAndTryB =
          stringValueByStringKey.toList.map(
            stringKeyAndStringValue => (
                (
                    stringKeyAndStringValue._1
                  , parserA(stringKeyAndStringValue._1)
                )
              , (
                    stringKeyAndStringValue._2
                  , parserB(stringKeyAndStringValue._2)
                )
            )
          )
        if (!stringKeyAndTryAAndStringValueAndTryB.exists(tuple2 => tuple2._1._2.isFailure && tuple2._2._2.isFailure))
          Success(stringKeyAndTryAAndStringValueAndTryB.map(tuple2 => (tuple2._1._2.get, tuple2._2._2.get)).toMap)
        else {
          val keyFailures =
            stringKeyAndTryAAndStringValueAndTryB.filter(_._1._2.isFailure).map(tuple2 => (tuple2._1._1, tuple2._1._2.asInstanceOf[Failure[_]]))
          val valueFailures =
            stringKeyAndTryAAndStringValueAndTryB.filter(_._2._2.isFailure).map(tuple2 => (tuple2._2._1, tuple2._2._2.asInstanceOf[Failure[_]]))
          val plural =
            if (keyFailures.size + valueFailures.size > 1) "s" else ""
          val keyFailureAsToString =
            keyFailures.map(tuple2 => s"key:${tuple2._1}=>${tuple2._2.exception.getMessage}")
          val valueFailureAsToString =
            valueFailures.map(tuple2 => s"value:${tuple2._1}=>${tuple2._2.exception.getMessage}")
          val failureAsToString =
            (keyFailureAsToString ::: valueFailureAsToString).mkString("|&&|")
          Failure(new IllegalStateException(s"parser failed on key/value pair$plural - $failureAsToString"))
        }
      }
    )
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
