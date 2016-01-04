/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.configuration                             **
**   Name:      ValueTypedMap.scala                                     **
**                                                                      **
** Description:                                                         **
**  Provides transform into a Map making sure the case sensitive        **
**  conversion doesn't cause key collision ambiguity                    **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.configuration

import scala.util.{Failure, Success, Try}

import org.scalaolio.collection.immutable.List_._

object ValueTypedMap {
  def apply(
      valueByKey: Map[String, String]
    , isKeyCaseSensitive: Boolean = false
    , templateWedge: (String => String) = (string) => string
  ): Try[ValueTypedMap] =
    validateAndConform(valueByKey, isKeyCaseSensitive).flatMap(
      valueByKeyValid =>
        Success(new Impl(valueByKeyValid, isKeyCaseSensitive, templateWedge))
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
    , val templateWedge: (String => String)
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
  def templateWedge: (String => String)

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
      value => {
        val valueNew =
          templateWedge(value)
        if (valueNew.nonEmpty)
          parse(valueNew)
        else
          if (emptyValueStringHasMeaning)
            Success(emptyValueStringMeans.apply())
          else
            Failure(new IllegalStateException(s"since emptyValueStringHasMeaning is false, key [$key] must not return value of empty String"))
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
