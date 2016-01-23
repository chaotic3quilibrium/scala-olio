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

object ValueTyped {
  val DEFAULT_EMPTY_VALUE_STRING_MEANS_BOOLEAN: Boolean = false
  val DEFAULT_EMPTY_VALUE_STRING_MEANS_BYTE: Byte = 0
  val DEFAULT_EMPTY_VALUE_STRING_MEANS_SHORT: Short = 0
  val DEFAULT_EMPTY_VALUE_STRING_MEANS_CHAR: Char = 0
  val DEFAULT_EMPTY_VALUE_STRING_MEANS_INT: Int = 0
  val DEFAULT_EMPTY_VALUE_STRING_MEANS_LONG: Long = 0L
  val DEFAULT_EMPTY_VALUE_STRING_MEANS_FLOAT: Float = 0.0f
  val DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE: Double = 0.0d
  val DEFAULT_EMPTY_VALUE_STRING_MEANS_STRING: String = ""
  val DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_INT: BigInt = BigInt(DEFAULT_EMPTY_VALUE_STRING_MEANS_INT)
  val DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_DECIMAL: BigDecimal = BigDecimal(DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE)
  val DEFAULT_VALUES_BOOLEAN_TRUE_LOWER_CASE: Set[String] = Set("Y", "Yes", "T", "True", "H", "High", "On").map(_.toLowerCase)
  val DEFAULT_DATE_TIME_ZONE: DateTimeZone = DateTimeZone.UTC
  val DEFAULT_LIST_STRING_SEPARATOR = ","
  val DEFAULT_SET_STRING_SEPARATOR = DEFAULT_LIST_STRING_SEPARATOR
  val DEFAULT_MAP_STRING_STRING_SEPARATOR = DEFAULT_LIST_STRING_SEPARATOR
  val DEFAULT_MAP_STRING_STRING_KEY_VALUE_SEPARATOR = "->"

  object Parsers {
    object Primitives {
      def parseBoolean(
          value: String
      )(
          implicit valuesBooleanTrueLowerCase: Set[String] = DEFAULT_VALUES_BOOLEAN_TRUE_LOWER_CASE
      ): Try[Option[Boolean]] =
        Try(valuesBooleanTrueLowerCase.contains(value.toLowerCase)).map(Some(_))

      def parseByte(value: String): Try[Option[Byte]] =
        Try(value.toByte).map(Some(_))

      def parseShort(value: String): Try[Option[Short]] =
        Try(value.toShort).map(Some(_))

      def parseChar(value: String): Try[Option[Char]] =
        Try(value.head).map(Some(_))

      def parseInt(value: String): Try[Option[Int]] =
        Try(value.toInt).map(Some(_))

      def parseLong(value: String): Try[Option[Long]] =
        Try(value.toLong).map(Some(_))

      def parseFloat(value: String): Try[Option[Float]] =
        Try(value.toFloat).map(Some(_))

      def parseDouble(value: String): Try[Option[Double]] =
        Try(value.toDouble).map(Some(_))
    }
    object Classes {
      object Singles {
        def parseString(value: String): Try[Option[String]] =
          Success(Some(value))

        def parseBigInt(value: String): Try[Option[BigInt]] =
          Try(BigInt(value)).map(Some(_))

        def parseBigDecimal(value: String): Try[Option[BigDecimal]] =
          Try(BigDecimal(value)).map(Some(_))

        def parseDateTime(
            value: String
        )(
            implicit dateTimeZone: DateTimeZone = DEFAULT_DATE_TIME_ZONE
        ): Try[Option[DateTime]] =
          Try(new DateTime(value, dateTimeZone)).map(Some(_))
        }
      object Collections {
        val functionReferenceTryOptionListParserIdentity: String => Try[Option[String]] =
          ValueTyped.Parsers.Classes.Singles.parseString

        val functionReferenceTryOptionMapParserIdentity: (String, String) => Try[Option[(String, String)]] =
          (stringA, stringB) =>
            Success(Some((stringA, stringB)))
      }
    }
  }
}

trait ValueTyped {
  def valueByKey: Map[String, String] //key always nonEmpty, value associated with key may be isEmpty or nonEmpty
  def isKeyCaseSensitive: Boolean //if false, valueByKey.keySet will all be force converted toLowerCase
  def tryOptionValueWedgeNonEmpty: (String, String) => Try[Option[String]] // = (keyAbsolute, value) => Success(Some(value))
  def tryOptionValueWedgeIsEmpty: (String, Boolean) => Try[Option[String]] // = (keyAbsolute, isValueEmptyString) => Success(Some(""))

  def tryOptionFromStringToTyped[A](
      key: String //key
//    , tryParseToA: String => Try[Option[A]] //parse
    , optionEmptyValueStringMeans: Option[A] //when Some(A), that A is the meaningful value; when None, return Failure indicating an undefined default value
    )(implicit
        parser: String => Try[Option[A]]
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
              parser(valueResolved).flatMap {
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

  object Primitives {
    //Boolean
    def tryOptionBoolean(
        key: String
      , optionEmptyValueStringMeans: Option[Boolean] =
          Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BOOLEAN) //set to None if empty value is undefined and should return a Failure//, valuesBooleanTrueLowerCase: Set[String] = DEFAULT_VALUES_BOOLEAN_TRUE_LOWER_CASE
    )(implicit
        parser: String => Try[Option[Boolean]] =
          ValueTyped.Parsers.Primitives.parseBoolean
    ): Try[Option[Boolean]] =
      tryOptionFromStringToTyped(key, optionEmptyValueStringMeans)(parser)

    def tryBoolean(
        key: String
      , optionEmptyValueStringMeans: Option[Boolean] =
          Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BOOLEAN) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Boolean]] =
          ValueTyped.Parsers.Primitives.parseBoolean
    ): Try[Boolean] =
      tryOptionBoolean(key, optionEmptyValueStringMeans).map(_.get)

    def optionBoolean(
        key: String
      , optionEmptyValueStringMeans: Option[Boolean] =
          Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BOOLEAN) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Boolean]] =
          ValueTyped.Parsers.Primitives.parseBoolean
    ): Option[Boolean] =
      tryOptionBoolean(key, optionEmptyValueStringMeans).get

    def boolean(
        key: String
      , optionEmptyValueStringMeans: Option[Boolean] =
          Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BOOLEAN) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Boolean]] =
          ValueTyped.Parsers.Primitives.parseBoolean
    ): Boolean =
      optionBoolean(key, optionEmptyValueStringMeans).get

    //Byte
    def tryOptionByte(
        key: String
      , optionEmptyValueStringMeans: Option[Byte] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BYTE) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Byte]] =
          ValueTyped.Parsers.Primitives.parseByte
    ): Try[Option[Byte]] =
      tryOptionFromStringToTyped(key, optionEmptyValueStringMeans)(parser)

    def tryByte(
        key: String
      , optionEmptyValueStringMeans: Option[Byte] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BYTE) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Byte]] =
          ValueTyped.Parsers.Primitives.parseByte
    ): Try[Byte] =
      tryOptionByte(key, optionEmptyValueStringMeans).map(_.get)

    def optionByte(
        key: String
      , optionEmptyValueStringMeans: Option[Byte] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BYTE) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Byte]] =
          ValueTyped.Parsers.Primitives.parseByte
    ): Option[Byte] =
      tryOptionByte(key, optionEmptyValueStringMeans).get

    def byte(
        key: String
      , optionEmptyValueStringMeans: Option[Byte] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BYTE) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Byte]] =
          ValueTyped.Parsers.Primitives.parseByte
    ): Byte =
      optionByte(key, optionEmptyValueStringMeans).get

    //Short
    def tryOptionShort(
        key: String
      , optionEmptyValueStringMeans: Option[Short] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_SHORT) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Short]] =
          ValueTyped.Parsers.Primitives.parseShort
    ): Try[Option[Short]] =
      tryOptionFromStringToTyped(key, optionEmptyValueStringMeans)(parser)

    def tryShort(
        key: String
      , optionEmptyValueStringMeans: Option[Short] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_SHORT) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Short]] =
          ValueTyped.Parsers.Primitives.parseShort
    ): Try[Short] =
      tryOptionShort(key, optionEmptyValueStringMeans).map(_.get)

    def optionShort(
        key: String
      , optionEmptyValueStringMeans: Option[Short] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_SHORT) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Short]] =
          ValueTyped.Parsers.Primitives.parseShort
    ): Option[Short] =
      tryOptionShort(key, optionEmptyValueStringMeans).get

    def short(
        key: String
      , optionEmptyValueStringMeans: Option[Short] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_SHORT) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Short]] =
          ValueTyped.Parsers.Primitives.parseShort
    ): Short =
      optionShort(key, optionEmptyValueStringMeans).get

    //Char
    def tryOptionChar(
        key: String
      , optionEmptyValueStringMeans: Option[Char] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_CHAR) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Char]] =
          ValueTyped.Parsers.Primitives.parseChar
    ): Try[Option[Char]] =
      tryOptionFromStringToTyped(key, optionEmptyValueStringMeans)(parser)

    def tryChar(
        key: String
      , optionEmptyValueStringMeans: Option[Char] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_CHAR) //set to None if empty value is undefined and should return a Failure
    ): Try[Char] =
      tryOptionChar(key, optionEmptyValueStringMeans).map(_.get)

    def optionChar(
        key: String
      , optionEmptyValueStringMeans: Option[Char] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_CHAR) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Char]] =
          ValueTyped.Parsers.Primitives.parseChar
    ): Option[Char] =
      tryOptionChar(key, optionEmptyValueStringMeans).get

    def char(
        key: String
      , optionEmptyValueStringMeans: Option[Char] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_CHAR) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Char]] =
          ValueTyped.Parsers.Primitives.parseChar
    ): Char =
      optionChar(key, optionEmptyValueStringMeans).get

    //Int
    def tryOptionInt(
        key: String
      , optionEmptyValueStringMeans: Option[Int] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_INT) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Int]] =
          ValueTyped.Parsers.Primitives.parseInt
    ): Try[Option[Int]] =
      tryOptionFromStringToTyped(key, optionEmptyValueStringMeans)(parser)

    def tryInt(
        key: String
      , optionEmptyValueStringMeans: Option[Int] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_INT) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Int]] =
          ValueTyped.Parsers.Primitives.parseInt
    ): Try[Int] =
      tryOptionInt(key, optionEmptyValueStringMeans).map(_.get)

    def optionInt(
        key: String
      , optionEmptyValueStringMeans: Option[Int] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_INT) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Int]] =
          ValueTyped.Parsers.Primitives.parseInt
    ): Option[Int] =
      tryOptionInt(key, optionEmptyValueStringMeans).get

    def int(
        key: String
      , optionEmptyValueStringMeans: Option[Int] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_INT) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Int]] =
          ValueTyped.Parsers.Primitives.parseInt
    ): Int =
      optionInt(key, optionEmptyValueStringMeans).get

    //Long
    def tryOptionLong(
        key: String
      , optionEmptyValueStringMeans: Option[Long] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_LONG) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Long]] =
          ValueTyped.Parsers.Primitives.parseLong
    ): Try[Option[Long]] =
      tryOptionFromStringToTyped(key, optionEmptyValueStringMeans)(parser)

    def tryLong(
        key: String
      , optionEmptyValueStringMeans: Option[Long] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_LONG) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Long]] =
          ValueTyped.Parsers.Primitives.parseLong
    ): Try[Long] =
      tryOptionLong(key, optionEmptyValueStringMeans).map(_.get)

    def optionLong(
        key: String
      , optionEmptyValueStringMeans: Option[Long] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_LONG) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Long]] =
          ValueTyped.Parsers.Primitives.parseLong
    ): Option[Long] =
      tryOptionLong(key, optionEmptyValueStringMeans).get

    def long(
        key: String
      , optionEmptyValueStringMeans: Option[Long] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_LONG) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Long]] =
          ValueTyped.Parsers.Primitives.parseLong
    ): Long =
      optionLong(key, optionEmptyValueStringMeans).get

    //Float
    def tryOptionFloat(
        key: String
      , optionEmptyValueStringMeans: Option[Float] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_FLOAT) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Float]] =
          ValueTyped.Parsers.Primitives.parseFloat
    ): Try[Option[Float]] =
      tryOptionFromStringToTyped(key, optionEmptyValueStringMeans)(parser)

    def tryFloat(
        key: String
      , optionEmptyValueStringMeans: Option[Float] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_FLOAT) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Float]] =
          ValueTyped.Parsers.Primitives.parseFloat
    ): Try[Float] =
      tryOptionFloat(key, optionEmptyValueStringMeans).map(_.get)

    def optionFloat(
        key: String
      , optionEmptyValueStringMeans: Option[Float] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_FLOAT) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Float]] =
          ValueTyped.Parsers.Primitives.parseFloat
    ): Option[Float] =
      tryOptionFloat(key, optionEmptyValueStringMeans).get

    def float(
        key: String
      , optionEmptyValueStringMeans: Option[Float] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_FLOAT) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Float]] =
          ValueTyped.Parsers.Primitives.parseFloat
    ): Float =
      optionFloat(key, optionEmptyValueStringMeans).get

    //Double
    def tryOptionDouble(
        key: String
      , optionEmptyValueStringMeans: Option[Double] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Double]] =
          ValueTyped.Parsers.Primitives.parseDouble
    ): Try[Option[Double]] =
      tryOptionFromStringToTyped(key, optionEmptyValueStringMeans)(parser)

    def tryDouble(
        key: String
      , optionEmptyValueStringMeans: Option[Double] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Double]] =
          ValueTyped.Parsers.Primitives.parseDouble
    ): Try[Double] =
      tryOptionDouble(key, optionEmptyValueStringMeans).map(_.get)

    def optionDouble(
        key: String
      , optionEmptyValueStringMeans: Option[Double] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Double]] =
          ValueTyped.Parsers.Primitives.parseDouble
    ): Option[Double] =
      tryOptionDouble(key, optionEmptyValueStringMeans).get

    def double(
        key: String
      , optionEmptyValueStringMeans: Option[Double] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_DOUBLE) //set to None if empty value is undefined and should return a Failure
    )(implicit
        parser: String => Try[Option[Double]] =
          ValueTyped.Parsers.Primitives.parseDouble
    ): Double =
      optionDouble(key, optionEmptyValueStringMeans).get
  }

  object Classes {
    object Singles {
        //String
      def tryOptionString(
          key: String
        , optionEmptyValueStringMeans: Option[String] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_STRING) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[String]] =
            ValueTyped.Parsers.Classes.Singles.parseString
      ): Try[Option[String]] =
        tryOptionFromStringToTyped(key, optionEmptyValueStringMeans)(parser)

      def tryString(
          key: String
        , optionEmptyValueStringMeans: Option[String] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_STRING) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[String]] =
            ValueTyped.Parsers.Classes.Singles.parseString
      ): Try[String] =
        tryOptionString(key, optionEmptyValueStringMeans).map(_.get)

      def optionString(
          key: String
        , optionEmptyValueStringMeans: Option[String] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_STRING) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[String]] =
            ValueTyped.Parsers.Classes.Singles.parseString
      ): Option[String] =
        tryOptionString(key, optionEmptyValueStringMeans).get

      def string(
          key: String
        , optionEmptyValueStringMeans: Option[String] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_STRING) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[String]] =
            ValueTyped.Parsers.Classes.Singles.parseString
      ): String =
        optionString(key, optionEmptyValueStringMeans).get

      //BigInt
      def tryOptionBigInt(
          key: String
        , optionEmptyValueStringMeans: Option[BigInt] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_INT) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[BigInt]] =
            ValueTyped.Parsers.Classes.Singles.parseBigInt
      ): Try[Option[BigInt]] =
        tryOptionFromStringToTyped(key, optionEmptyValueStringMeans)(parser)

      def tryBigInt(
          key: String
        , optionEmptyValueStringMeans: Option[BigInt] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_INT) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[BigInt]] =
            ValueTyped.Parsers.Classes.Singles.parseBigInt
      ): Try[BigInt] =
        tryOptionBigInt(key, optionEmptyValueStringMeans).map(_.get)

      def optionBigInt(
          key: String
        , optionEmptyValueStringMeans: Option[BigInt] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_INT) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[BigInt]] =
            ValueTyped.Parsers.Classes.Singles.parseBigInt
      ): Option[BigInt] =
        tryOptionBigInt(key, optionEmptyValueStringMeans).get

      def bigInt(
          key: String
        , optionEmptyValueStringMeans: Option[BigInt] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_INT) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[BigInt]] =
            ValueTyped.Parsers.Classes.Singles.parseBigInt
      ): BigInt =
        optionBigInt(key, optionEmptyValueStringMeans).get

      //BigDecimal
      def tryOptionBigDecimal(
          key: String
        , optionEmptyValueStringMeans: Option[BigDecimal] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_DECIMAL) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[BigDecimal]] =
            ValueTyped.Parsers.Classes.Singles.parseBigDecimal
      ): Try[Option[BigDecimal]] =
        tryOptionFromStringToTyped(key, optionEmptyValueStringMeans)(parser)

      def tryBigDecimal(
          key: String
        , optionEmptyValueStringMeans: Option[BigDecimal] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_DECIMAL) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[BigDecimal]] =
            ValueTyped.Parsers.Classes.Singles.parseBigDecimal
      ): Try[BigDecimal] =
        tryOptionBigDecimal(key, optionEmptyValueStringMeans).map(_.get)

      def optionBigDecimal(
          key: String
        , optionEmptyValueStringMeans: Option[BigDecimal] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_DECIMAL) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[BigDecimal]] =
            ValueTyped.Parsers.Classes.Singles.parseBigDecimal
      ): Option[BigDecimal] =
        tryOptionBigDecimal(key, optionEmptyValueStringMeans).get

      def bigDecimal(
          key: String
        , optionEmptyValueStringMeans: Option[BigDecimal] = Some(ValueTyped.DEFAULT_EMPTY_VALUE_STRING_MEANS_BIG_DECIMAL) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[BigDecimal]] =
            ValueTyped.Parsers.Classes.Singles.parseBigDecimal
      ): BigDecimal =
        optionBigDecimal(key, optionEmptyValueStringMeans).get

      //DateTime
      def tryOptionDateTime(
          key: String
        , optionEmptyValueStringMeans: Option[DateTime] = Some(new DateTime) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[DateTime]] =
            ValueTyped.Parsers.Classes.Singles.parseDateTime
      ): Try[Option[DateTime]] =
        tryOptionFromStringToTyped(key, optionEmptyValueStringMeans)(parser)

      def tryDateTime(
          key: String
        , optionEmptyValueStringMeans: Option[DateTime] = Some(new DateTime) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[DateTime]] =
            ValueTyped.Parsers.Classes.Singles.parseDateTime
      ): Try[DateTime] =
        tryOptionDateTime(key, optionEmptyValueStringMeans).map(_.get)

      def optionDateTime(
          key: String
        , optionEmptyValueStringMeans: Option[DateTime] = Some(new DateTime) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[DateTime]] =
            ValueTyped.Parsers.Classes.Singles.parseDateTime
      ): Option[DateTime] =
        tryOptionDateTime(key, optionEmptyValueStringMeans).get

      def dateTime(
          key: String
        , optionEmptyValueStringMeans: Option[DateTime] = Some(new DateTime) //set to None if empty value is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[DateTime]] =
            ValueTyped.Parsers.Classes.Singles.parseDateTime
      ): DateTime =
        optionDateTime(key, optionEmptyValueStringMeans).get
    }

    object Collections {
      //List
      private val functionReferenceTryOptionListParserIdentity: String => Try[Option[String]] =
        ValueTyped.Parsers.Classes.Collections.functionReferenceTryOptionListParserIdentity

      def tryOptionList[A](
          key: String
        , optionEmptyValueAsMeans: Option[List[A]] = Some(List[A]()) //set to None if empty valueAs undefined and should return a Failure
        , optionEmptyValueAMeans: Option[A] = None //set to None if empty string is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[A]]
        , separator: String = ValueTyped.DEFAULT_LIST_STRING_SEPARATOR
        , isSeparatorRegex: Boolean = false
      ): Try[Option[List[A]]] = {
        def parseString(values: String): Try[Option[List[String]]] =
          Try(
            if (isSeparatorRegex)
              values.split(separator).toList
            else
              values.splitLiterally(separator)
          ).map(Some(_))
        def keyResolved =
          if (isKeyCaseSensitive)
            key
          else
            key.toLowerCase
        def resolveEmptyValues: Try[Option[List[A]]] =
          optionEmptyValueAsMeans match {
            case Some(as) =>
              Success(Some(as))
            case None =>
              Failure(new IllegalStateException(s"when optionEmptyValueAsMeans.isEmpty, valueAs returned by key [$keyResolved] must be nonEmpty"))
          }
        def resolveEmptyValueA(optionAs: List[Option[A]]): Try[Option[List[A]]] =
          if (!optionAs.exists(_.isEmpty))
            Success(Some(optionAs.flatten))
          else
            optionEmptyValueAMeans match {
              case Some(emptyValueAMeans) =>
                Success(
                  Some(
                    optionAs.map {
                      case Some(a) =>
                        a
                      case None =>
                        emptyValueAMeans
                    }
                  )
                )
              case None =>
                Failure(new IllegalStateException(s"when optionEmptyValueMeans.isEmpty, each string value in strings returned by key [$keyResolved] must be nonEmpty"))
            }
        ( if (parser == functionReferenceTryOptionListParserIdentity) {
            tryOptionFromStringToTyped(key, Some(List[String]()))(parseString)
          }
          else
            tryOptionList(key, Some(List[String]()), if (optionEmptyValueAMeans.isDefined) Some("") else None)(functionReferenceTryOptionListParserIdentity, separator, isSeparatorRegex)
        ).flatMap {
          case Some(valueAs) =>
            val valueAndTryOptionAs =
              valueAs.map(value => (value, parser(value)))
            if (!valueAndTryOptionAs.exists(_._2.isFailure)) {
              val optionAs =
                valueAndTryOptionAs.collect {
                  case (_, Success(optionA)) =>
                    optionA
                }
              if (optionAs.nonEmpty)
                resolveEmptyValueA(optionAs)
              else
                resolveEmptyValues
            }
            else {
              val failureAs =
                valueAndTryOptionAs.collect {
                  case (v, f: Failure[_]) =>
                    (v, f)
                }
              val plural =
                if (failureAs.size > 1) "s" else ""
              val failureAsToString =
                failureAs.map(tuple2 => s"${tuple2._1}=>${tuple2._2.exception.getMessage}").mkString("|&&|")
              Failure(new IllegalStateException(s"parser failed on value$plural - $failureAsToString"))
            }
          case None =>
            resolveEmptyValues
        }
      }

      def tryList[A](
          key: String
        , optionEmptyValueAsMeans: Option[List[A]] = Some(List[A]()) //set to None if empty valueAs undefined and should return a Failure
        , optionEmptyValueAMeans: Option[A] = None //set to None if empty string is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[A]]
        , separator: String = ValueTyped.DEFAULT_LIST_STRING_SEPARATOR
        , isSeparatorRegex: Boolean = false
      ): Try[List[A]] =
        tryOptionList(key, optionEmptyValueAsMeans)(parser, separator, isSeparatorRegex).map(_.get)

      def optionList[A](
          key: String
        //, parse: String => Try[Option[A]]
        , optionEmptyValueAsMeans: Option[List[A]] = Some(List[A]()) //set to None if empty valueAs undefined and should return a Failure
        , optionEmptyValueAMeans: Option[A] = None //set to None if empty string is undefined and should return a Failure
      )(implicit
          parse: String => Try[Option[A]]
        , separator: String = ValueTyped.DEFAULT_LIST_STRING_SEPARATOR
        , isSeparatorRegex: Boolean = false
      ): Option[List[A]] =
        tryOptionList(key, optionEmptyValueAsMeans)(parse, separator, isSeparatorRegex).get

      def list[A](
          key: String
        //, parse: String => Try[Option[A]]
        , optionEmptyValueAsMeans: Option[List[A]] = Some(List[A]()) //set to None if empty valueAs undefined and should return a Failure
        , optionEmptyValueAMeans: Option[A] = None //set to None if empty string is undefined and should return a Failure
      )(implicit
          parse: String => Try[Option[A]]
        , separator: String = ValueTyped.DEFAULT_LIST_STRING_SEPARATOR
        , isSeparatorRegex: Boolean = false
      ): List[A] =
        optionList(key, optionEmptyValueAsMeans)(parse, separator, isSeparatorRegex).get

      //Set
      def tryOptionSet[A](
          key: String
        , optionEmptyValueAsMeans: Option[Set[A]] = Some(Set[A]()) //set to None if empty valueAs undefined and should return a Failure
        , optionEmptyValueAMeans: Option[A] = None //set to None if empty string is undefined and should return a Failure
      )(implicit
          parse: String => Try[Option[A]]
        , separator: String = ValueTyped.DEFAULT_SET_STRING_SEPARATOR
        , isSeparatorRegex: Boolean = false
      ): Try[Option[Set[A]]] = {
        def keyResolved =
          if (isKeyCaseSensitive)
            key
          else
            key.toLowerCase
        def resolveEmptyValues: Try[Option[Set[A]]] =
          optionEmptyValueAsMeans match {
            case Some(as) =>
              Success(Some(as))
            case None =>
              Failure(new IllegalStateException(s"when optionEmptyValueAsMeans.isEmpty, strings returned by key [$keyResolved] must be nonEmpty"))
          }
        tryOptionList[A](key, Some(List[A]()), optionEmptyValueAMeans)(parse, separator, isSeparatorRegex).map(_.map(_.toSet)).flatMap {
          case Some(as) =>
            if (as.nonEmpty)
              Success(Some(as))
            else
              resolveEmptyValues
          case None =>
            resolveEmptyValues
        }
      }

      def trySet[A](
          key: String
        , optionEmptyValueAsMeans: Option[Set[A]] = Some(Set[A]()) //set to None if empty valueAs undefined and should return a Failure
        , optionEmptyValueAMeans: Option[A] = None //set to None if empty string is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[A]]
        , separator: String = ValueTyped.DEFAULT_SET_STRING_SEPARATOR
        , isSeparatorRegex: Boolean = false
      ): Try[Set[A]] =
        tryOptionSet(key, optionEmptyValueAsMeans, optionEmptyValueAMeans)(parser, separator, isSeparatorRegex).map(_.get)

      def optionSet[A](
          key: String
        , optionEmptyValueAsMeans: Option[Set[A]] = Some(Set[A]()) //set to None if empty valueAs undefined and should return a Failure
        , optionEmptyValueAMeans: Option[A] = None //set to None if empty string is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[A]]
        , separator: String = ValueTyped.DEFAULT_SET_STRING_SEPARATOR
        , isSeparatorRegex: Boolean = false
      ): Option[Set[A]] =
        tryOptionSet(key, optionEmptyValueAsMeans, optionEmptyValueAMeans)(parser, separator, isSeparatorRegex).get

      def set[A](
          key: String
        , optionEmptyValueAsMeans: Option[Set[A]] = Some(Set[A]()) //set to None if empty valueAs undefined and should return a Failure
        , optionEmptyValueAMeans: Option[A] = None //set to None if empty string is undefined and should return a Failure
      )(implicit
          parser: String => Try[Option[A]]
        , separator: String = ValueTyped.DEFAULT_SET_STRING_SEPARATOR
        , isSeparatorRegex: Boolean = false
      ): Set[A] =
        optionSet(key, optionEmptyValueAsMeans, optionEmptyValueAMeans)(parser, separator, isSeparatorRegex).get

      private val functionReferenceTryOptionMapParserIdentity: (String, String) => Try[Option[(String, String)]] =
        ValueTyped.Parsers.Classes.Collections.functionReferenceTryOptionMapParserIdentity

      def tryOptionMap[A, B](
          key: String
        , optionEmptyValueABsMeans: Option[Map[A, B]] = Some(Map[A, B]()) //set to None if empty valueABs undefined and should return a Failure
        , optionEmptyValueABMeans: Option[(A, B)] = None //set to None if empty string is undefined and should return a Failure
      )(implicit
          parser: (String, String) => Try[Option[(A, B)]]
        , separator: String = ValueTyped.DEFAULT_MAP_STRING_STRING_SEPARATOR
        , isSeparatorRegex: Boolean = false
        , keyValueSeparator: String = ValueTyped.DEFAULT_MAP_STRING_STRING_KEY_VALUE_SEPARATOR
        , isKeyValueSeparatorRegex: Boolean = false
      ): Try[Option[Map[A, B]]] = {
        def parseKeyAndValue(keyAndValue: String): Try[Option[(String, String)]] =
          if (keyAndValue.indexOf(keyValueSeparator) > -1)
            if (isKeyValueSeparatorRegex) {
              val tokens =
                keyAndValue.split(keyValueSeparator).toList
              if (tokens.size == 2)
                Success(Some(tokens.head, tokens(2)))
              else
                Failure(new IllegalStateException(s"unable to use keyValueSeparator [$keyValueSeparator] to regex split keyAndValue [$keyAndValue]"))
            }
            else
              Success(Some(keyAndValue.spanSansSeparator(keyValueSeparator)))
          else
            Failure(new IllegalStateException(s"unable to find keyValueSeparator [$keyValueSeparator] in keyAndValue [$keyAndValue]"))
        def keyResolved =
          if (isKeyCaseSensitive)
            key
          else
            key.toLowerCase
        def resolveEmptyValues: Try[Option[Map[A, B]]] =
          optionEmptyValueABsMeans match {
            case Some(abs) =>
              Success(Some(abs))
            case None =>
              Failure(new IllegalStateException(s"when optionEmptyValueAsMeans.isEmpty, abs returned by key [$keyResolved] must be nonEmpty"))
          }
        def resolveEmptyValueAB(optionABs: List[Option[(A, B)]]): Try[Option[Map[A, B]]] =
          optionEmptyValueABMeans match {
            case Some(emptyValueABMeans) =>
              Success(
                Some(
                  optionABs.map {
                    case Some(ab) =>
                      ab
                    case None =>
                      emptyValueABMeans
                  }.toMap
                )
              )
            case None =>
              Failure(new IllegalStateException(s"when optionEmptyValueMeans.isEmpty, each string value in strings returned by key [$keyResolved] must be nonEmpty"))
          }
        ( if (parser == ValueTyped.Parsers.Classes.Collections.functionReferenceTryOptionMapParserIdentity)
            tryOptionList(key, Some(List[(String, String)]()), if (optionEmptyValueABMeans.isDefined) Some(("", "")) else None)(parseKeyAndValue, separator, isSeparatorRegex).map(_.map(_.toMap))
          else
            tryOptionMap(key, Some(Map[String, String]()), if (optionEmptyValueABMeans.isDefined) Some(("", "")) else None)(functionReferenceTryOptionMapParserIdentity, separator, isSeparatorRegex, keyValueSeparator, isKeyValueSeparatorRegex)
        ).flatMap {
          case Some(stringValueByStringKey) =>
            val stringKeyAndStringValueAndTryOptionAAndBs =
              stringValueByStringKey.toList.map(
                stringKeyAndStringValue =>
                  (stringKeyAndStringValue, parser(stringKeyAndStringValue._1, stringKeyAndStringValue._2))
              )
            if (!stringKeyAndStringValueAndTryOptionAAndBs.exists(t => t._2.isFailure)) {
              val optionABs =
                stringKeyAndStringValueAndTryOptionAAndBs.collect {
                  case (_, Success(optionAAndB)) =>
                    optionAAndB
                }
              if (optionABs.nonEmpty) {
                if (!optionABs.exists(_.isEmpty))
                  Success(
                    Some(
                      optionABs.collect {
                        case Some(aB) =>
                          aB
                      }.toMap
                    )
                  )
                else
                  resolveEmptyValueAB(optionABs)
              }
              else
                resolveEmptyValues
            }
            else {
              val stringKeyAndStringValueAndFailures =
                stringKeyAndStringValueAndTryOptionAAndBs.collect {
                  case (stringKeyAndStringValue, f: Failure[_]) =>
                    (stringKeyAndStringValue, f)
                }
              val plural =
                if (stringKeyAndStringValueAndFailures.size > 1) "s" else ""
              val stringAndFailuresToString =
                stringKeyAndStringValueAndFailures.map {
                  case ((sa: String, sb: String), f: Failure[_]) =>
                    s"(key,value):($sa,$sb)=>${f.exception.getMessage}"
                }.mkString("|&&|")
              Failure(new IllegalStateException(s"parser failed on key/value pair$plural - $stringAndFailuresToString"))
            }
          case None =>
            resolveEmptyValues
        }
      }

      def tryMap[A, B](
          key: String
        , optionEmptyValueABsMeans: Option[Map[A, B]] = Some(Map[A, B]()) //set to None if empty valueABs undefined and should return a Failure
        , optionEmptyValueABMeans: Option[(A, B)] = None //set to None if empty string is undefined and should return a Failure
      )(implicit
          parser: (String, String) => Try[Option[(A, B)]]
        , separator: String = ValueTyped.DEFAULT_MAP_STRING_STRING_SEPARATOR
        , isSeparatorRegex: Boolean = false
        , keyValueSeparator: String = ValueTyped.DEFAULT_MAP_STRING_STRING_KEY_VALUE_SEPARATOR
        , isKeyValueSeparatorRegex: Boolean = false
      ): Try[Map[A, B]] =
        tryOptionMap(key, optionEmptyValueABsMeans, optionEmptyValueABMeans)(parser, separator, isSeparatorRegex, keyValueSeparator, isKeyValueSeparatorRegex).map(_.get)

      def optionMap[A, B](
          key: String
        , optionEmptyValueABsMeans: Option[Map[A, B]] = Some(Map[A, B]()) //set to None if empty valueABs undefined and should return a Failure
        , optionEmptyValueABMeans: Option[(A, B)] = None //set to None if empty string is undefined and should return a Failure
      )(implicit
          parser: (String, String) => Try[Option[(A, B)]]
        , separator: String = ValueTyped.DEFAULT_MAP_STRING_STRING_SEPARATOR
        , isSeparatorRegex: Boolean = false
        , keyValueSeparator: String = ValueTyped.DEFAULT_MAP_STRING_STRING_KEY_VALUE_SEPARATOR
        , isKeyValueSeparatorRegex: Boolean = false
      ): Option[Map[A, B]] =
        tryOptionMap(key, optionEmptyValueABsMeans, optionEmptyValueABMeans)(parser, separator, isSeparatorRegex, keyValueSeparator, isKeyValueSeparatorRegex).get

      def map[A, B](
          key: String
        , optionEmptyValueABsMeans: Option[Map[A, B]] = Some(Map[A, B]()) //set to None if empty valueABs undefined and should return a Failure
        , optionEmptyValueABMeans: Option[(A, B)] = None //set to None if empty string is undefined and should return a Failure
      )(implicit
          parser: (String, String) => Try[Option[(A, B)]]
        , separator: String = ValueTyped.DEFAULT_MAP_STRING_STRING_SEPARATOR
        , isSeparatorRegex: Boolean = false
        , keyValueSeparator: String = ValueTyped.DEFAULT_MAP_STRING_STRING_KEY_VALUE_SEPARATOR
        , isKeyValueSeparatorRegex: Boolean = false
      ): Map[A, B] =
        optionMap(key, optionEmptyValueABsMeans, optionEmptyValueABMeans)(parser, separator, isSeparatorRegex, keyValueSeparator, isKeyValueSeparatorRegex).get
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
