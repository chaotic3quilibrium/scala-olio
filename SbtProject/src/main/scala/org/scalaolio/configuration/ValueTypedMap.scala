package org.scalaolio.configuration

import scala.util.{Failure, Success, Try}

import org.scalaolio.collection.immutable.List_._

object ValueTypedMap {
  def apply(
      valueByKey: Map[String, String]
    , isKeyCaseSensitive: Boolean = false
  ): Try[ValueTypedMap] =
    validateAndConform(valueByKey, isKeyCaseSensitive).flatMap(
      valueByKeyValid =>
        Success(new Impl(valueByKeyValid, isKeyCaseSensitive))
    )

  def validateAndConform(
      valueByKey: Map[String, String]
    , isKeyCaseSensitive: Boolean
  ): Try[(Map[String, String])] =
    if (valueByKey.nonEmpty)
      if (!valueByKey.exists(_._1.isEmpty))
        if (isKeyCaseSensitive)
          Success(valueByKey)
        else {
          val (_, dupes) =
            valueByKey.keySet.toList.map(_.toLowerCase).filterDupes
          if (dupes.isEmpty)
            Success(valueByKey.map(keyAndValue => (keyAndValue._1.toLowerCase, keyAndValue._2)))
          else
            Failure(new IllegalArgumentException(s"valueByKey.keySet must not have duplicates following the toLowerCase conversion [${dupes.map(_._1).mkString(",")}]"))
        }
      else
        Failure(new IllegalArgumentException("valueByKey must not have a key consisting of an empty String"))
    else
      Failure(new IllegalArgumentException("valueByKey must be nonEmpty"))

  private[ValueTypedMap] class Impl private[ValueTypedMap] (
      val valueByKey: Map[String, String]
    , val isKeyCaseSensitive: Boolean
  ) extends ValueTypedMap {
    override def toString: String =
      s"ValueTypedMap(${valueByKey.mkString(",")},$isKeyCaseSensitive)"

    def invertKeyCaseSensitive(newKeys: Option[Set[String]]): Try[ValueTypedMap] =
      if (isKeyCaseSensitive)
        transitionToNotCaseSensitive
      else
        transitionToCaseSensitive(newKeys)

    def transitionToCaseSensitive(newKeys: Option[Set[String]]): Try[ValueTypedMap] =
      if (!isKeyCaseSensitive)
        newKeys match {
          case Some(newKeysGet) =>
            val (distinctLowerCase, dupesLowerCase) =
              newKeysGet.toList.map(_.toLowerCase).filterDupes
            if (dupesLowerCase.isEmpty)
              if (distinctLowerCase.toSet == valueByKey.keySet)
                ValueTypedMap(newKeysGet.map(key => (key, valueByKey(key.toLowerCase))).toMap, isKeyCaseSensitive = true)
              else
                //TODO: elaborate the diff
                Failure(new IllegalStateException(s"when newKeys isDefined and following the toLowerCase conversion, it must be equal to valueByKey.keySet"))
            else
              Failure(new IllegalStateException(s"when newKeys isDefined and following the toLowerCase conversion, it must not have duplicates [${dupesLowerCase.map(_._1).mkString(",")}]"))
          case None =>
            ValueTypedMap(valueByKey, isKeyCaseSensitive = true) //no new keys provided, reuse the existing (all-lower-case) keys
        }
      else
        Success(this) //isKeyCaseSensitive is already true, nothing to do

    def transitionToNotCaseSensitive: Try[ValueTypedMap] =
      if (isKeyCaseSensitive) {
        val (_, dupesLowerCase) =
          valueByKey.keySet.toList.map(_.toLowerCase).filterDupes
        if (dupesLowerCase.isEmpty)
          ValueTypedMap(valueByKey.map(keyAndValue => (keyAndValue._1.toLowerCase, keyAndValue._2)), isKeyCaseSensitive = false)
        else
          Failure(new IllegalStateException(s"following the toLowerCase conversion, valueByKey.keySet must not have duplicates [${dupesLowerCase.map(_._1).mkString(",")}]"))
      }
      else
        Success(this) //isKeyCaseSensitive is already false, nothing to do
  }
}

trait ValueTypedMap extends ValueTyped {
  def valueByKey: Map[String, String] //key always nonEmpty, value associated with key may be isEmpty or nonEmpty
  def isKeyCaseSensitive: Boolean //if false, valueByKey.keySet will all be force converted toLowerCase

  def invertKeyCaseSensitive(newKeys: Option[Set[String]] = None): Try[ValueTypedMap]
  def transitionToCaseSensitive(newKeys: Option[Set[String]] = None): Try[ValueTypedMap]
  def transitionToNotCaseSensitive: Try[ValueTypedMap]

  def fromStringToTyped[A](
      key: String
    , parse: String => Try[A]
    , emptyValueStringHasMeaning: Boolean = false
    , emptyValueStringMeans: () => A  //only relevant if emptyValueStringHasMeaning is true
  ): Try[A] =
    Try(valueByKey(if (isKeyCaseSensitive) key else key.toLowerCase)).flatMap(
      value =>
        if (value.nonEmpty)
          parse(value)
        else
          if (emptyValueStringHasMeaning)
            Success(emptyValueStringMeans.apply())
          else
            Failure(new IllegalStateException(s"since emptyValueStringHasMeaning is false, key [$key] must not return value of empty String"))
    )
}
