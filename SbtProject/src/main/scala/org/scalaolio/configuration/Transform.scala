package org.scalaolio.configuration

import scala.util.{Failure, Success, Try}

import org.scalaolio.collection.immutable.List_._

object Transform {
  def apply(
      valueTypedMap: ValueTypedMap
  ): Transform =
    new Impl(valueTypedMap, valueTypedMap.valueByKey.keySet.toList.sorted)

  def apply(
      valueTypedMap: ValueTypedMap
    , keysOrdered: List[String]
  ): Try[Transform] =
    validateAndConform(valueTypedMap, keysOrdered).flatMap(
      valueTypedMapAndKeysOrdered =>
        Success(new Impl(valueTypedMapAndKeysOrdered._1, valueTypedMapAndKeysOrdered._2))
    )

  def apply(
      keyAndValues: List[(String, String)]
    , isKeyCaseSensitive: Boolean = false
  ): Try[Transform] = {
    ValueTypedMap(keyAndValues.toMap, isKeyCaseSensitive).flatMap(
      valueTypedMap =>
        Transform(valueTypedMap, keyAndValues.map(_._1))
    )
  }

  def validateAndConform(
      valueTypedMap: ValueTypedMap
    , keysOrdered: List[String]
  ): Try[(ValueTypedMap, List[String])] =
    if (keysOrdered.nonEmpty) {
      val keysOrderedLowerCase = keysOrdered.map(_.toLowerCase)
      val (_, dupes) =
        if (valueTypedMap.isKeyCaseSensitive)
          keysOrdered.filterDupes
        else
          keysOrderedLowerCase.filterDupes
      if (dupes.isEmpty) {
        val keySetLowerCase = valueTypedMap.valueByKey.keySet.map(_.toLowerCase)
        val keysOrderedDiff =
          if (valueTypedMap.isKeyCaseSensitive)
            keysOrdered.filter(!valueTypedMap.valueByKey.keySet.contains(_))
          else
            keysOrderedLowerCase.filter(!keySetLowerCase.contains(_))
        if (keysOrderedDiff.isEmpty) {
          val keySetDiff =
            if (valueTypedMap.isKeyCaseSensitive)
              valueTypedMap.valueByKey.keySet.filter(!keysOrdered.contains(_))
            else
              keySetLowerCase.filter(!keysOrderedLowerCase.contains(_))
          if (keySetDiff.isEmpty)
            if (valueTypedMap.isKeyCaseSensitive)
              Success((valueTypedMap, keysOrdered))
            else
              ValueTypedMap(valueTypedMap.valueByKey.map(tuple2 => (tuple2._1.toLowerCase, tuple2._2)), isKeyCaseSensitive = false).flatMap(
                valueTypedMapGet =>
                  Success((valueTypedMapGet, keysOrdered))
              )
          else
            Failure(new IllegalArgumentException(s"all of keyValue.keySet must be contained within keysOrdered [${keySetDiff.mkString(",")}]"))
        }
        else
          Failure(new IllegalArgumentException(s"all of keysOrdered must be contained within keyValue.keySet [${keysOrderedDiff.mkString(",")}]"))
      }
      else
        Failure(new IllegalArgumentException(s"keysOrdered must not contain duplicates [${dupes.mkString(",")}]"))
    }
    else
      Failure(new IllegalArgumentException("keysOrdered must be nonEmpty"))

  private[Transform] class Impl private[Transform] (
      val valueTypedMap: ValueTypedMap
    , val keysOrdered: List[String]
  ) extends Transform {
    val keyAndValues: List[(String, String)] =
      keysOrdered.map(key => (key, valueTypedMap.valueByKey(if (valueTypedMap.isKeyCaseSensitive) key else key.toLowerCase)))

    override def toString: String = {
      s"Transform($keyAndValues,${valueTypedMap.isKeyCaseSensitive})"
    }

    def rebase(insertKeyPrefix: String): Transform =
      new Impl(
          ValueTypedMap(
              valueTypedMap.valueByKey.map(keyAndValue => (if (valueTypedMap.isKeyCaseSensitive) insertKeyPrefix else insertKeyPrefix.toLowerCase + keyAndValue._1, keyAndValue._2))
            , valueTypedMap.isKeyCaseSensitive
          ).get
        , keysOrdered.map(insertKeyPrefix + _)
      )

    def subset(keyPrefix: String, retainKeyPrefix: Boolean = false): Try[Transform] = {
      val keysOrderedSubset =
        if (keyPrefix.nonEmpty)
          if (valueTypedMap.isKeyCaseSensitive)
            keysOrdered.filter(_.startsWith(keyPrefix))
          else {
            val keyPrefixToLowerCase =
              keyPrefix.toLowerCase
            keysOrdered.filter(_.toLowerCase.startsWith(keyPrefixToLowerCase))
          }
        else
          keysOrdered
      if (keysOrderedSubset.nonEmpty)
        if (!keysOrderedSubset.contains(keyPrefix)) {
          val keyAndValues: List[(String, (String, String))] =
            keysOrderedSubset.map(
              key => {
                val newKeyOrdered =
                  if (retainKeyPrefix)
                    key
                  else
                    key.drop(keyPrefix.length)
                val (newKeyAccessMap, newKeyMap) =
                  if (valueTypedMap.isKeyCaseSensitive)
                    (key, newKeyOrdered)
                  else
                    (key.toLowerCase, newKeyOrdered.toLowerCase)
                (
                    newKeyOrdered
                  , (
                        newKeyMap
                      , valueTypedMap.valueByKey(newKeyAccessMap)
                    )
                )
              }
            )
          ValueTypedMap(
              keyAndValues.map(_._2).toMap
            , valueTypedMap.isKeyCaseSensitive
          ).flatMap(
            mapping =>
              Success(new Impl(mapping, keyAndValues.map(_._1)))
          )
        }
        else
          Failure(new IllegalArgumentException(s"keysOrdered contains a key equal to keyPrefix [$keyPrefix]"))
      else
        Failure(new IllegalArgumentException(s"keysOrdered isEmpty after filtering for keyPrefix [$keyPrefix]"))
    }

    def invertCaseSensitive: Try[Transform] =
      valueTypedMap.invertKeyCaseSensitive(Some(keysOrdered.toSet)).flatMap(
        valueTypedMapGet =>
          Success(new Impl(valueTypedMapGet, keysOrdered))
      )

    def merge(that: Transform): Try[Transform] =
      if (that == this)
        Success(this) //since it is the same instance, just return this
      else
        (
          if (that.valueTypedMap.isKeyCaseSensitive == valueTypedMap.isKeyCaseSensitive)
            Success(that)
          else
            that.invertCaseSensitive
        ).flatMap(
          thatNew => {
            val (keysOrderedAdd, valueByKeyAdd) =
              if (valueTypedMap.isKeyCaseSensitive) {
                val keysOrderedAdd =
                  thatNew.keysOrdered.filter(!keysOrdered.contains(_))
                val valueByKeyAdd =
                  keysOrderedAdd.map(key => (key, thatNew.valueTypedMap.valueByKey(key))).toMap
                (keysOrderedAdd, valueByKeyAdd)
              }
              else {
                val keysOrderedAddLowerCase = {
                  val thisKeysOrderedLowerCase =
                    keysOrdered.map(_.toLowerCase)
                  val thatKeysOrderedLowerCase =
                    thatNew.keysOrdered.map(_.toLowerCase)
                  thatKeysOrderedLowerCase.filter(!thisKeysOrderedLowerCase.contains(_))
                }
                val keysOrderedAdd =
                  thatNew.keysOrdered.filter(key => keysOrderedAddLowerCase.contains(key.toLowerCase))
                val valueByKeyAdd =
                  keysOrderedAddLowerCase.map(key => (key, thatNew.valueTypedMap.valueByKey(key))).toMap
                (keysOrderedAdd, valueByKeyAdd)
              }
            ValueTypedMap(valueTypedMap.valueByKey ++ valueByKeyAdd, valueTypedMap.isKeyCaseSensitive).flatMap(
              mapping =>
                Success(new Impl(mapping, keysOrdered ++ keysOrderedAdd))
            )
          }
        )
  }
}

trait Transform {
  def valueTypedMap: ValueTypedMap
  def keysOrdered: List[String] //always nonEmpty

  def rebase(insertKeyPrefix: String): Transform //iterates through each of the keys, prepending insertKeyPrefix to each key
  def subset(keyPrefix: String, retainKeyPrefix: Boolean = false): Try[Transform] //retains ordering
  def invertCaseSensitive: Try[Transform] //changes the case sensitivity to the opposite of this.isCaseSensitive
  def merge(that: Transform): Try[Transform] //appends only key/value pairs from that which don't already exist in this - this.isCaseSensitive is used
}
