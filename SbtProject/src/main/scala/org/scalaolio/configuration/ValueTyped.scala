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

  def valueByKey: Map[String, String] //key always nonEmpty, value associated with key may be isEmpty or nonEmpty
  def isKeyCaseSensitive: Boolean //if false, valueByKey.keySet will all be force converted toLowerCase
  def tryOptionValueWedgeNonEmpty: (String, String) => Try[Option[String]] // = (keyAbsolute, value) => Success(Some(value))
  def tryOptionValueWedgeIsEmpty: (String, Boolean) => Try[Option[String]] // = (keyAbsolute, isValueEmptyString) => Success(Some(""))

  def tryOptionFromStringToTyped[A](
      key: String //key
    , tryParseToA: String => Try[Option[A]] //parse
    , optionEmptyValueStringMeans: Option[A] //when Some(A), that A is the meaningful value; when None, return Failure indicating an undefined default value
  ): Try[Option[A]] = {
    val keyResolved =
      if (isKeyCaseSensitive)
        key
      else
        key.toLowerCase
    def resolveEmptyValue: Try[Option[A]] =
      optionEmptyValueStringMeans match {
        case Some(a) =>
          Success(Some(a))
        case None =>
          Failure(new IllegalStateException(s"when optionEmptyValueStringMeans.isEmpty, value returned by key [$keyResolved] must be nonEmpty"))
      }
    def processNonEmpty(value: String) =
      for {
        optionValueResolved <-
          tryOptionValueWedgeNonEmpty(keyResolved, value)
        optionAResolved <-
          optionValueResolved match {
            case Some(valueResolved) =>
              tryParseToA(valueResolved).flatMap {
                case Some(aParsed) =>
                  Success(Some(aParsed))
                case None =>
                  resolveEmptyValue
              }
            case None =>
              resolveEmptyValue
          }
      } yield optionAResolved
    def processEmptyOrUndefined(isValueEmptyString: Boolean) =
      tryOptionValueWedgeIsEmpty(keyResolved, isValueEmptyString).flatMap {
        case Some(value) =>
          if (value.nonEmpty)
            processNonEmpty(value)
          else
            resolveEmptyValue
        case None =>
          resolveEmptyValue
      }
    valueByKey.get(keyResolved) match {
      case Some(value) =>
        if (value.nonEmpty)
          processNonEmpty(value)
        else
          processEmptyOrUndefined(true)
      case None =>
        processEmptyOrUndefined(false)
    }
  }

  //Simple - primitives
  //Boolean
  def tryOptionBoolean(
      key: String
    , optionEmptyValueStringMeans: Option[Boolean] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BOOLEAN) //set to None if empty value undefined and should return a Failure
    , valuesBooleanTrueLowerCase: Set[String] = DEFAULT_VALUES_BOOLEAN_TRUE_LOWER_CASE
  ): Try[Option[Boolean]] =
    tryOptionFromStringToTyped(key, value => Try(valuesBooleanTrueLowerCase.contains(value.toLowerCase)).map(Some(_)), optionEmptyValueStringMeans)

  def tryBoolean(
      key: String
    , optionEmptyValueStringMeans: Option[Boolean] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BOOLEAN) //set to None if empty value undefined and should return a Failure
    , valuesBooleanTrueLowerCase: Set[String] = DEFAULT_VALUES_BOOLEAN_TRUE_LOWER_CASE
  ): Try[Boolean] =
    tryOptionBoolean(key, optionEmptyValueStringMeans, valuesBooleanTrueLowerCase).map(_.get)

  def optionBoolean(
      key: String
    , optionEmptyValueStringMeans: Option[Boolean] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BOOLEAN) //set to None if empty value undefined and should return a Failure
    , valuesBooleanTrueLowerCase: Set[String] = DEFAULT_VALUES_BOOLEAN_TRUE_LOWER_CASE
  ): Option[Boolean] =
    tryOptionBoolean(key, optionEmptyValueStringMeans, valuesBooleanTrueLowerCase).get

  def boolean(
      key: String
    , optionEmptyValueStringMeans: Option[Boolean] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BOOLEAN) //set to None if empty value undefined and should return a Failure
    , valuesBooleanTrueLowerCase: Set[String] = DEFAULT_VALUES_BOOLEAN_TRUE_LOWER_CASE
  ): Boolean =
    optionBoolean(key, optionEmptyValueStringMeans, valuesBooleanTrueLowerCase).get

  //Byte
  def tryOptionByte(
      key: String
    , optionEmptyValueStringMeans: Option[Byte] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BYTE) //set to None if empty value undefined and should return a Failure
  ): Try[Option[Byte]] =
    tryOptionFromStringToTyped(key, value => Try(value.toByte).map(Some(_)), optionEmptyValueStringMeans)

  def tryByte(
      key: String
    , optionEmptyValueStringMeans: Option[Byte] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BYTE) //set to None if empty value undefined and should return a Failure
  ): Try[Byte] =
    tryOptionByte(key, optionEmptyValueStringMeans).map(_.get)

  def optionByte(
      key: String
    , optionEmptyValueStringMeans: Option[Byte] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BYTE) //set to None if empty value undefined and should return a Failure
  ): Option[Byte] =
    tryOptionByte(key, optionEmptyValueStringMeans).get

  def byte(
      key: String
    , optionEmptyValueStringMeans: Option[Byte] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BYTE) //set to None if empty value undefined and should return a Failure
  ): Byte =
    optionByte(key, optionEmptyValueStringMeans).get

  //Short
  def tryOptionShort(
      key: String
    , optionEmptyValueStringMeans: Option[Short] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_SHORT) //set to None if empty value undefined and should return a Failure
  ): Try[Option[Short]] =
    tryOptionFromStringToTyped(key, value => Try(value.toShort).map(Some(_)), optionEmptyValueStringMeans)

  def tryShort(
      key: String
    , optionEmptyValueStringMeans: Option[Short] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_SHORT) //set to None if empty value undefined and should return a Failure
  ): Try[Short] =
    tryOptionShort(key, optionEmptyValueStringMeans).map(_.get)

  def optionShort(
      key: String
    , optionEmptyValueStringMeans: Option[Short] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_SHORT) //set to None if empty value undefined and should return a Failure
  ): Option[Short] =
    tryOptionShort(key, optionEmptyValueStringMeans).get

  def short(
      key: String
    , optionEmptyValueStringMeans: Option[Short] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_SHORT) //set to None if empty value undefined and should return a Failure
  ): Short =
    optionShort(key, optionEmptyValueStringMeans).get

  //Char
  def tryOptionChar(
      key: String
    , optionEmptyValueStringMeans: Option[Char] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_CHAR) //set to None if empty value undefined and should return a Failure
  ): Try[Option[Char]] =
    tryOptionFromStringToTyped(key, value => Try(value.head).map(Some(_)), optionEmptyValueStringMeans)

  def tryChar(
      key: String
    , optionEmptyValueStringMeans: Option[Char] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_CHAR) //set to None if empty value undefined and should return a Failure
  ): Try[Char] =
    tryOptionChar(key, optionEmptyValueStringMeans).map(_.get)

  def optionChar(
      key: String
    , optionEmptyValueStringMeans: Option[Char] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_CHAR) //set to None if empty value undefined and should return a Failure
  ): Option[Char] =
    tryOptionChar(key, optionEmptyValueStringMeans).get

  def char(
      key: String
    , optionEmptyValueStringMeans: Option[Char] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_CHAR) //set to None if empty value undefined and should return a Failure
  ): Char =
    optionChar(key, optionEmptyValueStringMeans).get

  //Int
  def tryOptionInt(
      key: String
    , optionEmptyValueStringMeans: Option[Int] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_INT) //set to None if empty value undefined and should return a Failure
  ): Try[Option[Int]] =
    tryOptionFromStringToTyped(key, value => Try(value.toInt).map(Some(_)), optionEmptyValueStringMeans)

  def tryInt(
      key: String
    , optionEmptyValueStringMeans: Option[Int] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_INT) //set to None if empty value undefined and should return a Failure
  ): Try[Int] =
    tryOptionInt(key, optionEmptyValueStringMeans).map(_.get)

  def optionInt(
      key: String
    , optionEmptyValueStringMeans: Option[Int] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_INT) //set to None if empty value undefined and should return a Failure
  ): Option[Int] =
    tryOptionInt(key, optionEmptyValueStringMeans).get

  def int(
      key: String
    , optionEmptyValueStringMeans: Option[Int] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_INT) //set to None if empty value undefined and should return a Failure
  ): Int =
    optionInt(key, optionEmptyValueStringMeans).get

  //Long
  def tryOptionLong(
      key: String
    , optionEmptyValueStringMeans: Option[Long] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_LONG) //set to None if empty value undefined and should return a Failure
  ): Try[Option[Long]] =
    tryOptionFromStringToTyped(key, value => Try(value.toLong).map(Some(_)), optionEmptyValueStringMeans)

  def tryLong(
      key: String
    , optionEmptyValueStringMeans: Option[Long] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_LONG) //set to None if empty value undefined and should return a Failure
  ): Try[Long] =
    tryOptionLong(key, optionEmptyValueStringMeans).map(_.get)

  def optionLong(
      key: String
    , optionEmptyValueStringMeans: Option[Long] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_LONG) //set to None if empty value undefined and should return a Failure
  ): Option[Long] =
    tryOptionLong(key, optionEmptyValueStringMeans).get

  def long(
      key: String
    , optionEmptyValueStringMeans: Option[Long] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_LONG) //set to None if empty value undefined and should return a Failure
  ): Long =
    optionLong(key, optionEmptyValueStringMeans).get

  //Float
  def tryOptionFloat(
      key: String
    , optionEmptyValueStringMeans: Option[Float] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_FLOAT) //set to None if empty value undefined and should return a Failure
  ): Try[Option[Float]] =
    tryOptionFromStringToTyped(key, value => Try(value.toFloat).map(Some(_)), optionEmptyValueStringMeans)

  def tryFloat(
      key: String
    , optionEmptyValueStringMeans: Option[Float] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_FLOAT) //set to None if empty value undefined and should return a Failure
  ): Try[Float] =
    tryOptionFloat(key, optionEmptyValueStringMeans).map(_.get)

  def optionFloat(
      key: String
    , optionEmptyValueStringMeans: Option[Float] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_FLOAT) //set to None if empty value undefined and should return a Failure
  ): Option[Float] =
    tryOptionFloat(key, optionEmptyValueStringMeans).get

  def float(
      key: String
    , optionEmptyValueStringMeans: Option[Float] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_FLOAT) //set to None if empty value undefined and should return a Failure
  ): Float =
    optionFloat(key, optionEmptyValueStringMeans).get

  //Double
  def tryOptionDouble(
      key: String
    , optionEmptyValueStringMeans: Option[Double] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE) //set to None if empty value undefined and should return a Failure
  ): Try[Option[Double]] =
    tryOptionFromStringToTyped(key, value => Try(value.toDouble).map(Some(_)), optionEmptyValueStringMeans)

  def tryDouble(
      key: String
    , optionEmptyValueStringMeans: Option[Double] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE) //set to None if empty value undefined and should return a Failure
  ): Try[Double] =
    tryOptionDouble(key, optionEmptyValueStringMeans).map(_.get)

  def optionDouble(
      key: String
    , optionEmptyValueStringMeans: Option[Double] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE) //set to None if empty value undefined and should return a Failure
  ): Option[Double] =
    tryOptionDouble(key, optionEmptyValueStringMeans).get

  def double(
      key: String
    , optionEmptyValueStringMeans: Option[Double] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE) //set to None if empty value undefined and should return a Failure
  ): Double =
    optionDouble(key, optionEmptyValueStringMeans).get


  //simple - Classes
  //String
  def tryOptionString(
      key: String
    , optionEmptyValueStringMeans: Option[String] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_STRING) //set to None if empty value undefined and should return a Failure
  ): Try[Option[String]] =
    tryOptionFromStringToTyped(key, value => Success(value).map(Some(_)), optionEmptyValueStringMeans)

  //TODO: must apply this pattern to all the other calls
  def tryString(
      key: String
    , optionEmptyValueStringMeans: Option[String] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_STRING) //set to None if empty value undefined and should return a Failure
  ): Try[String] =
    tryOptionString(key, optionEmptyValueStringMeans).map(_.get)

  //TODO: must apply this pattern to all the other calls
  def optionString(
      key: String
    , optionEmptyValueStringMeans: Option[String] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_STRING) //set to None if empty value undefined and should return a Failure
  ): Option[String] =
    tryOptionString(key, optionEmptyValueStringMeans).get

  //TODO: must apply this pattern to all the other calls
  def string(
      key: String
    , optionEmptyValueStringMeans: Option[String] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_STRING) //set to None if empty value undefined and should return a Failure
  ): String =
    optionString(key, optionEmptyValueStringMeans).get

  //BigInt
  def tryOptionBigInt(
      key: String
    , optionEmptyValueStringMeans: Option[BigInt] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_INT) //set to None if empty value undefined and should return a Failure
  ): Try[Option[BigInt]] =
    tryOptionFromStringToTyped(key, value => Try(BigInt(value)).map(Some(_)), optionEmptyValueStringMeans)

  def tryBigInt(
      key: String
    , optionEmptyValueStringMeans: Option[BigInt] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_INT) //set to None if empty value undefined and should return a Failure
  ): Try[BigInt] =
    tryOptionBigInt(key, optionEmptyValueStringMeans).map(_.get)

  def optionBigInt(
      key: String
    , optionEmptyValueStringMeans: Option[BigInt] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_INT) //set to None if empty value undefined and should return a Failure
  ): Option[BigInt] =
    tryOptionBigInt(key, optionEmptyValueStringMeans).get

  def bigInt(
      key: String
    , optionEmptyValueStringMeans: Option[BigInt] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_INT) //set to None if empty value undefined and should return a Failure
  ): BigInt =
    optionBigInt(key, optionEmptyValueStringMeans).get

  //BigDecimal
  def tryOptionBigDecimal(
      key: String
    , optionEmptyValueStringMeans: Option[BigDecimal] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_DECIMAL) //set to None if empty value undefined and should return a Failure
  ): Try[Option[BigDecimal]] =
    tryOptionFromStringToTyped(key, value => Try(BigDecimal(value)).map(Some(_)), optionEmptyValueStringMeans)

  def tryBigDecimal(
      key: String
    , optionEmptyValueStringMeans: Option[BigDecimal] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_DECIMAL) //set to None if empty value undefined and should return a Failure
  ): Try[BigDecimal] =
    tryOptionBigDecimal(key, optionEmptyValueStringMeans).map(_.get)

  def optionBigDecimal(
      key: String
    , optionEmptyValueStringMeans: Option[BigDecimal] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_DECIMAL) //set to None if empty value undefined and should return a Failure
  ): Option[BigDecimal] =
    tryOptionBigDecimal(key, optionEmptyValueStringMeans).get

  def bigDecimal(
      key: String
    , optionEmptyValueStringMeans: Option[BigDecimal] = Some(DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_DECIMAL) //set to None if empty value undefined and should return a Failure
  ): BigDecimal =
    optionBigDecimal(key, optionEmptyValueStringMeans).get

  //DateTime
  def tryOptionDateTime(
      key: String
    , optionEmptyValueStringMeans: Option[DateTime] = Some(new DateTime) //set to None if empty value undefined and should return a Failure
  ): Try[Option[DateTime]] =
    tryOptionFromStringToTyped(key, value => Try(new DateTime(value)).map(Some(_)), optionEmptyValueStringMeans)

  def tryDateTime(
      key: String
    , optionEmptyValueStringMeans: Option[DateTime] = Some(new DateTime) //set to None if empty value undefined and should return a Failure
  ): Try[DateTime] =
    tryOptionDateTime(key, optionEmptyValueStringMeans).map(_.get)

  def optionDateTime(
      key: String
    , optionEmptyValueStringMeans: Option[DateTime] = Some(new DateTime) //set to None if empty value undefined and should return a Failure
  ): Option[DateTime] =
    tryOptionDateTime(key, optionEmptyValueStringMeans).get

  def dateTime(
      key: String
    , optionEmptyValueStringMeans: Option[DateTime] = Some(new DateTime) //set to None if empty value undefined and should return a Failure
  ): DateTime =
    optionDateTime(key, optionEmptyValueStringMeans).get

  //UtcDateTime
  def tryOptionUtcDateTime(
      key: String
    , optionEmptyValueStringMeans: Option[DateTime] = Some(new DateTime(DateTimeZone.UTC)) //set to None if empty value undefined and should return a Failure
  ): Try[Option[DateTime]] =
    tryOptionFromStringToTyped(key, value => Try(new DateTime(value, DateTimeZone.UTC)).map(Some(_)), optionEmptyValueStringMeans)

  def tryUtcDateTime(
      key: String
    , optionEmptyValueStringMeans: Option[DateTime] = Some(new DateTime(DateTimeZone.UTC)) //set to None if empty value undefined and should return a Failure
  ): Try[DateTime] =
    tryOptionUtcDateTime(key, optionEmptyValueStringMeans).map(_.get)

  def optionUtcDateTime(
      key: String
    , optionEmptyValueStringMeans: Option[DateTime] = Some(new DateTime(DateTimeZone.UTC)) //set to None if empty value undefined and should return a Failure
  ): Option[DateTime] =
    tryOptionUtcDateTime(key, optionEmptyValueStringMeans).get

  def utcDateTime(
      key: String
    , optionEmptyValueStringMeans: Option[DateTime] = Some(new DateTime(DateTimeZone.UTC)) //set to None if empty value undefined and should return a Failure
  ): DateTime =
    optionUtcDateTime(key, optionEmptyValueStringMeans).get

  //complex - classes
  //TODO: add the three other methods tryX, optionX and X (like all the types above)
  //TODO: add parameter and implementation fix for when single value is empty; i.e. provide default or generate Failure
  def tryOptionListString(
      key: String
    , optionEmptyValuesMeans: Option[List[String]] = Some(Nil) //set to None if empty values undefined and should return a Failure
    , separator: String = DEFAULT_LIST_STRING_SEPARATOR
    , isSeparatorRegex: Boolean = false
  ): Try[Option[List[String]]] = {
    def resolveEmptyValue: Try[Option[List[String]]] =
      optionEmptyValuesMeans match {
        case Some(strings) =>
          Success(Some(strings))
        case None =>
          val keyResolved =
            if (isKeyCaseSensitive)
              key
            else
              key.toLowerCase
          Failure(new IllegalStateException(s"when optionEmptyValueAsMeans.isEmpty, strings returned by key [$keyResolved] must be nonEmpty"))
      }
    def f(value: String): Try[Option[List[String]]] =
      Try(
        if (isSeparatorRegex)
          value.split(separator).toList
        else
          value.splitLiterally(separator)
      ).map(Some(_))
    tryOptionFromStringToTyped(key, f, Some(Nil)).flatMap {
      case Some(strings) =>
        if (strings.nonEmpty)
          Success(Some(strings))
        else
          resolveEmptyValue
      case None =>
        resolveEmptyValue
    }
  }

  //TODO: add the three other methods tryX, optionX and X (like all the types above)
  //TODO: add parameter and implementation fix for when single value is empty; i.e. provide default or generate Failure
  def tryOptionList[A](
      key: String
    , tryParseToA: String => Try[A]
    , optionEmptyValueAsMeans: Option[List[A]] = Some(Nil) //set to None if empty valueAs undefined and should return a Failure
    , separator: String = DEFAULT_LIST_STRING_SEPARATOR
    , isSeparatorRegex: Boolean = false
  ): Try[Option[List[A]]] = {
    def resolveEmptyValue: Try[Option[List[A]]] =
      optionEmptyValueAsMeans match {
        case Some(as) =>
          Success(Some(as))
        case None =>
          val keyResolved =
            if (isKeyCaseSensitive)
              key
            else
              key.toLowerCase
          Failure(new IllegalStateException(s"when optionEmptyValueAsMeans.isEmpty, valueAs returned by key [$keyResolved] must be nonEmpty"))
      }
    tryOptionListString(key, Some(Nil), separator, isSeparatorRegex).flatMap {
      case Some(valueAs) =>
        val valueAndTryAs =
          valueAs.map(value => (value, tryParseToA(value)))
        if (!valueAndTryAs.exists(_._2.isFailure)) {
          val as =
            valueAndTryAs.collect {
              case (_, Success(a)) =>
                a
            }
          if (as.nonEmpty)
            Success(Some(as))
          else
            resolveEmptyValue
        }
        else {
          val failureAs =
            valueAndTryAs.collect {
              case (v, f: Failure[A]) =>
                (v, f)
            }
          val plural =
            if (failureAs.size > 1) "s" else ""
          val failureAsToString =
            failureAs.map(tuple2 => s"${tuple2._1}=>${tuple2._2.exception.getMessage}").mkString("|&&|")
          Failure(new IllegalStateException(s"parser failed on value$plural - $failureAsToString"))
        }
      case None =>
        resolveEmptyValue
    }
  }

  //TODO: add the three other methods tryX, optionX and X (like all the types above)
  //TODO: add parameter and implementation fix for when single value is empty; i.e. provide default or generate Failure
  def tryOptionSetString(
      key: String
    , optionEmptyValuesMeans: Option[Set[String]] = Some(Set()) //set to None if empty values undefined and should return a Failure
    , separator: String = DEFAULT_SET_STRING_SEPARATOR
    , isSeparatorRegex: Boolean = false
  ): Try[Option[Set[String]]] = {
    def resolveEmptyValue: Try[Option[Set[String]]] =
      optionEmptyValuesMeans match {
        case Some(strings) =>
          Success(Some(strings))
        case None =>
          val keyResolved =
            if (isKeyCaseSensitive)
              key
            else
              key.toLowerCase
          Failure(new IllegalStateException(s"when optionEmptyValueAsMeans.isEmpty, strings returned by key [$keyResolved] must be nonEmpty"))
      }
    tryOptionListString(key, Some(Nil), separator, isSeparatorRegex).map(_.map(_.toSet)).flatMap {
      case Some(strings) =>
        if (strings.nonEmpty)
          Success(Some(strings))
        else
          resolveEmptyValue
      case None =>
        resolveEmptyValue
    }
  }

  //TODO: add the three other methods tryX, optionX and X (like all the types above)
  //TODO: add parameter and implementation fix for when single value is empty; i.e. provide default or generate Failure
  def tryOptionSet[A](
      key: String
    , tryParseToA: String => Try[A]
    , optionEmptyValueAsMeans: Option[Set[A]] = Some(Set()) //set to None if empty valueAs undefined and should return a Failure
    , separator: String = DEFAULT_SET_STRING_SEPARATOR
    , isSeparatorRegex: Boolean = false
  ): Try[Option[Set[A]]] = {
    def resolveEmptyValue: Try[Option[Set[A]]] =
      optionEmptyValueAsMeans match {
        case Some(as) =>
          Success(Some(as))
        case None =>
          val keyResolved =
            if (isKeyCaseSensitive)
              key
            else
              key.toLowerCase
          Failure(new IllegalStateException(s"when optionEmptyValueAsMeans.isEmpty, strings returned by key [$keyResolved] must be nonEmpty"))
      }
    tryOptionList[A](key, tryParseToA, Some(Nil), separator, isSeparatorRegex).map(_.map(_.toSet)).flatMap {
      case Some(as) =>
        if (as.nonEmpty)
          Success(Some(as))
        else
          resolveEmptyValue
      case None =>
        resolveEmptyValue
    }
  }

  //TODO: add the three other methods tryX, optionX and X (like all the types above)
  //TODO: add parameter and implementation fix for when single value is empty; i.e. provide default or generate Failure
  def tryOptionMapStringString(
      key: String
    , optionEmptyValuesMeans: Option[Map[String, String]] = Some(Map()) //set to None if empty values undefined and should return a Failure
    , separator: String = DEFAULT_MAP_STRING_STRING_SEPARATOR
    , isSeparatorRegex: Boolean = false
    , keyValueSeparator: String = DEFAULT_MAP_STRING_STRING_KEY_VALUE_SEPARATOR
    , isKeyValueSeparatorRegex: Boolean = false
  ): Try[Option[Map[String, String]]] = {
    def resolveEmptyValue: Try[Option[Map[String, String]]] =
      optionEmptyValuesMeans match {
        case Some(stringByString) =>
          Success(Some(stringByString))
        case None =>
          val keyResolved =
            if (isKeyCaseSensitive)
              key
            else
              key.toLowerCase
          Failure(new IllegalStateException(s"when optionEmptyValueAsMeans.isEmpty, stringByString returned by key [$keyResolved] must be nonEmpty"))
      }
    tryOptionListString(key, Some(Nil), separator, isSeparatorRegex).flatMap {
      case Some(listStringKeyAndValue) =>
        val listTryUnparsedAndTryParsed =
          listStringKeyAndValue.map(
            keyAndValue => {
              val parsed =
                if (isKeyValueSeparatorRegex)
                  keyAndValue.split(keyValueSeparator).toList
                else
                  keyAndValue.splitLiterally(keyValueSeparator)
              val tryTuple2StringString: Try[(String, String)] =
                if (parsed.size == 2) {
                  val (key, value) =
                    (parsed.head, parsed(1))
                  if (key.nonEmpty)
//                    if (value.nonEmpty || emptyMapValueHasMeaning)
                      Success((key, value))
//                    else
//                      Failure(new IllegalStateException(s"for keyAndValue [$keyAndValue] using keyValueSeparator [$keyValueSeparator], when emptyMapValueHasMeaning is false the parsed value must be nonEmpty"))
                  else
                    Failure(new IllegalStateException(s"for keyAndValue [$keyAndValue] using keyValueSeparator [$keyValueSeparator], the parsed key must be nonEmpty"))
                }
                else
                  Failure(new IllegalStateException(s"for keyAndValue [$keyAndValue] using keyValueSeparator [$keyValueSeparator], parsed.length [${parsed.length}] was not equal to 2"))
              (keyAndValue, tryTuple2StringString)
            }
          )
        if (!listTryUnparsedAndTryParsed.exists(_._2.isFailure)) {
          val stringByString =
            listTryUnparsedAndTryParsed.collect {
              case (_, Success(stringAndString)) =>
                stringAndString
            }.toMap
          if (stringByString.nonEmpty)
            Success(Some(stringByString))
          else
            resolveEmptyValue
        }
        else {
          val failureStringAndStrings =
            listTryUnparsedAndTryParsed.collect {
              case (v, f: Failure[(String, String)]) =>
                (v, f)
            }
          val plural =
            if (failureStringAndStrings.size > 1) "s" else ""
          val failureStringAndStringsToString =
            failureStringAndStrings.map(tuple2 => s"${tuple2._1}=>${tuple2._2.exception.getMessage}").mkString("|&&|")
          Failure(new IllegalStateException(s"parser failed on key/value pair$plural - $failureStringAndStringsToString"))
        }
      case None =>
        resolveEmptyValue
    }
  }

  //TODO: add the three other methods tryX, optionX and X (like all the types above)
  //TODO: add parameter and implementation fix for when single value is empty; i.e. provide default or generate Failure
  def tryOptionMap[A, B](
      key: String
    , tryParseToA: String => Try[A]
    , tryParseToB: String => Try[B]
    , optionEmptyValueABsMeans: Option[Map[A, B]] = Some(Map()) //set to None if empty valueABs undefined and should return a Failure
    , separator: String = DEFAULT_MAP_STRING_STRING_SEPARATOR
    , isSeparatorRegex: Boolean = false
    , keyValueSeparator: String = DEFAULT_MAP_STRING_STRING_KEY_VALUE_SEPARATOR
    , isKeyValueSeparatorRegex: Boolean = false
  ): Try[Option[Map[A, B]]] = {
    def resolveEmptyValue: Try[Option[Map[A, B]]] =
      optionEmptyValueABsMeans match {
        case Some(abs) =>
          Success(Some(abs))
        case None =>
          val keyResolved =
            if (isKeyCaseSensitive)
              key
            else
              key.toLowerCase
          Failure(new IllegalStateException(s"when optionEmptyValueAsMeans.isEmpty, abs returned by key [$keyResolved] must be nonEmpty"))
      }
    tryOptionMapStringString(key, Some(Map()), separator, isSeparatorRegex, keyValueSeparator, isKeyValueSeparatorRegex).flatMap {
      case Some(stringValueByStringKey) =>
        val stringKeyAndTryAAndStringValueAndTryBs =
          stringValueByStringKey.toList.map(
            stringKeyAndStringValue => (
                (
                    stringKeyAndStringValue._1
                  , tryParseToA(stringKeyAndStringValue._1)
                )
              , (
                    stringKeyAndStringValue._2
                  , tryParseToB(stringKeyAndStringValue._2)
                )
            )
          )
        if (!stringKeyAndTryAAndStringValueAndTryBs.exists(tuple2 => tuple2._1._2.isFailure || tuple2._2._2.isFailure)) {
          val bByA =
            stringKeyAndTryAAndStringValueAndTryBs.collect {
              case ((_, Success(a)), (_, Success(b))) =>
                (a, b)
            }.toMap
          if (bByA.nonEmpty)
            Success(Some(bByA))
          else
            resolveEmptyValue
        }
        else {
          val stringAndFailures =
            stringKeyAndTryAAndStringValueAndTryBs.collect {
              case ((sa, fa: Failure[A]), (sb, fb: Failure[B])) =>
                List((sa, fa), (sa, sb, fb))
              case ((sa, fa: Failure[A]), _) =>
                List((sa, fa))
              case ((sa, _: Success[A]), (sb, fb: Failure[B])) =>
                List((sa, sb, fb))
            }.flatten
          val plural =
            if (stringAndFailures.size > 1) "s" else ""
          val stringAndFailuresToString =
            stringAndFailures.map {
              case (sa: String, fa: Failure[_]) =>
                s"key:$sa=>${fa.exception.getMessage}"
              case (sa: String, sb: String, fb: Failure[_]) =>
                s"(key,value):($sa,$sb)=>${fb.exception.getMessage}"
            }.mkString("|&&|")
          Failure(new IllegalStateException(s"parser failed on key/value pair$plural - $stringAndFailuresToString"))
        }
      case None =>
        resolveEmptyValue
    }
  }
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
