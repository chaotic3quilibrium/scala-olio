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
  //TODO: move to the Scala case class pattern
  def tryApply(
      valueByKey: Map[String, String]
    , isKeyCaseSensitive: Boolean = false
    , tryOptionValueWedgeNonEmpty: (String, String) => Try[Option[String]] =
        (key, value) =>
          Success(Some(value))
    , tryOptionValueWedgeIsEmpty: (String, Boolean) => Try[Option[String]] =
        (key, isValueEmptyString) =>
          Success(Some(""))
  ): Try[ValueTypedMap] =
    tryValidateAndConform(valueByKey, isKeyCaseSensitive).flatMap(
      valueByKeyValid =>
        Success(new Impl(valueByKeyValid, isKeyCaseSensitive, tryOptionValueWedgeNonEmpty, tryOptionValueWedgeIsEmpty))
    )

  def tryValidateAndConform(
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
    , override val tryOptionValueWedgeNonEmpty: (String, String) => Try[Option[String]]
    , override val tryOptionValueWedgeIsEmpty: (String, Boolean) => Try[Option[String]]
  ) extends ValueTypedMap {
    override def toString: String =
      s"ValueTypedMap(${valueByKey.mkString(",")},$isKeyCaseSensitive)"

    def tryInvertKeyCaseSensitive(newKeys: Option[Set[String]]): Try[ValueTypedMap] =
      if (isKeyCaseSensitive)
        tryTransitionToNotCaseSensitive
      else
        tryTransitionToCaseSensitive(newKeys)

    def tryTransitionToCaseSensitive(newKeys: Option[Set[String]]): Try[ValueTypedMap] =
      if (!isKeyCaseSensitive)
        newKeys match {
          case Some(newKeysGet) =>
            val (distinctLowerCase, dupesLowerCase) =
              newKeysGet.toList.map(_.toLowerCase).filterDupes
            if (dupesLowerCase.isEmpty)
              if (distinctLowerCase.toSet == valueByKey.keySet)
                ValueTypedMap.tryApply(newKeysGet.map(key => (key, valueByKey(key.toLowerCase))).toMap, isKeyCaseSensitive = true, tryOptionValueWedgeNonEmpty, tryOptionValueWedgeIsEmpty)
              else
                //TODO: elaborate the diff
                Failure(new IllegalStateException(s"when newKeys isDefined and following the toLowerCase conversion, it must be equal to valueByKey.keySet"))
            else
              Failure(new IllegalStateException(s"when newKeys isDefined and following the toLowerCase conversion, it must not have duplicates [${dupesLowerCase.map(_._1).mkString(",")}]"))
          case None =>
            ValueTypedMap.tryApply(valueByKey, isKeyCaseSensitive = true, tryOptionValueWedgeNonEmpty, tryOptionValueWedgeIsEmpty) //no new keys provided, reuse the existing (all-lower-case) keys
        }
      else
        Success(this) //isKeyCaseSensitive is already true, nothing to do

    def tryTransitionToNotCaseSensitive: Try[ValueTypedMap] =
      if (isKeyCaseSensitive) {
        val (_, dupesLowerCase) =
          valueByKey.keySet.toList.map(_.toLowerCase).filterDupes
        if (dupesLowerCase.isEmpty)
          ValueTypedMap.tryApply(valueByKey.map(keyAndValue => (keyAndValue._1.toLowerCase, keyAndValue._2)), isKeyCaseSensitive = false, tryOptionValueWedgeNonEmpty, tryOptionValueWedgeIsEmpty)
        else
          Failure(new IllegalStateException(s"following the toLowerCase conversion, valueByKey.keySet must not have duplicates [${dupesLowerCase.map(_._1).mkString(",")}]"))
      }
      else
        Success(this) //isKeyCaseSensitive is already false, nothing to do
  }
}

trait ValueTypedMap extends ValueTyped {
  def tryInvertKeyCaseSensitive(newKeys: Option[Set[String]] = None): Try[ValueTypedMap]
  def tryTransitionToCaseSensitive(newKeys: Option[Set[String]] = None): Try[ValueTypedMap]
  def tryTransitionToNotCaseSensitive: Try[ValueTypedMap]
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
