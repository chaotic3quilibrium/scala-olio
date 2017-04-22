/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.configuration                             **
**   Name:      Subset.scala                                            **
**                                                                      **
** Description:                                                         **
**  Generic interface to set of configuration options at any depth in a **
**  configuration tree                                                  **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.configuration

import scala.util.{Success, Try}

object Subset {
  //TODO: transition to Scala case class pattern
  def tryApply(
      nexus: Nexus
    , keyPrefix: String
    , retainKeyPrefix: Boolean = false
  ): Try[Subset] =
    validateAndConform(nexus, keyPrefix, retainKeyPrefix).flatMap(
      nexusAndKeyPrefixAndRetainKeyPrefix => {
        val transformDateTimeStamped =
          nexus.currentTransformDateTimeStamped
        transformDateTimeStamped.transform.trySubset(keyPrefix, retainKeyPrefix).flatMap(
          transformSubset =>
            Nexus.TransformDateTimeStamped.tryApply(transformSubset, transformDateTimeStamped.utcDateTimeStamp).flatMap(
              nexusTransformDateTimeStamped =>
                Success(
                  new Subset(
                      nexusAndKeyPrefixAndRetainKeyPrefix._1
                    , nexusAndKeyPrefixAndRetainKeyPrefix._2
                    , nexusAndKeyPrefixAndRetainKeyPrefix._3
                    , nexusTransformDateTimeStamped
                  )
                )
            )
        )
      }
    )

  def validateAndConform(nexus: Nexus, keyPrefix: String, retainKeyPrefix: Boolean): Try[(Nexus, String, Boolean)] =
    Success((nexus, keyPrefix, retainKeyPrefix))
}

class Subset private[Subset] (
    val nexus: Nexus
  , val keyPrefix: String
  , val retainKeyPrefix: Boolean
  , private val transformDateTimeStamped: Nexus.TransformDateTimeStamped //already filtered/truncated and non-empty
) {
  private def tryOptionValueWedgeNonEmpty(wedgeKeyAbsolute: String, wedgeValue: String): Try[Option[String]] =
    nexus.templateProfile match {
      case Some(templateProfile) =>
        if (wedgeValue.nonEmpty && wedgeValue.startsWith(templateProfile.prefix)) {
          val (wedgeValueSanTemplatePrefix, keys) =
            templateProfile.findKeys(wedgeValue)
          if (keys.nonEmpty) {
            val keysResolved =
              if (transformDateTimeStamped.transform.valueTypedMap.isKeyCaseSensitive)
                keys
              else
                keys.map(_.toLowerCase)
            val (valueByKeyRelative, keysRemainingAfterRelative) = {
              val knvr =
                keysResolved
                  .map(
                    key =>
                      (key, transformDateTimeStamped.transform.valueTypedMap.valueByKey.get(key))
                  )
                  .filter(_._2.isDefined)
                  .map(tuple => (tuple._1, tuple._2.get))
                  .toMap
              (knvr, keysResolved -- knvr.keySet)
            }
            val (valueByKeyAbsolute, keysRemainingAfterAbsolute) =
              if (keysRemainingAfterRelative.nonEmpty && keyPrefix.nonEmpty && !retainKeyPrefix) {
                val knva =
                  keysRemainingAfterRelative
                    .map(
                      key =>
                        (key, nexus.currentTransformDateTimeStamped.transform.valueTypedMap.valueByKey.get(key))
                    )
                    .filter(_._2.isDefined)
                    .map(tuple => (tuple._1, tuple._2.get))
                    .toMap
                (knva, keysRemainingAfterRelative -- knva.keySet)
              }
              else
                (Map[String, String](), keysRemainingAfterRelative)
            val (valueByKeyException, keysRemainingAfterException) =
              if (keysRemainingAfterAbsolute.nonEmpty)
                nexus.optionValueWedgeNonEmptyTemplateExceptionResolver match {
                  case Some(valueWedgeNonEmptyTemplateExceptionResolver) =>
                    val knve: Map[String, String] = {
                      ( for {
                          templateKey <-
                            keysRemainingAfterAbsolute
                        } yield (templateKey, valueWedgeNonEmptyTemplateExceptionResolver(this, wedgeKeyAbsolute, templateKey, wedgeValueSanTemplatePrefix))
                      )
                        .filter(_._2.isDefined)
                        .map(tuple => (tuple._1, tuple._2.get))
                        .toMap
                    }
                    (knve, keysRemainingAfterAbsolute -- knve.keySet)
                  case None =>
                    (Map[String, String](), keysRemainingAfterAbsolute)
                }
              else
                (Map[String, String](), keysRemainingAfterAbsolute)
            //TODO: must change move this to some form of logging
            println(s"Subset.tryOptionValueWedgeNonEmpty.Some(templateProfile).keysRemainingAfterException=Set(${"\"" + keysRemainingAfterException.toList.sorted.mkString("\",\"") + "\""})")
            val valueByKey = (
                 valueByKeyRelative
              ++ valueByKeyAbsolute
              ++ valueByKeyException
            )
            Success(Some(templateProfile.replace(wedgeValue, valueByKey, transformDateTimeStamped.transform.valueTypedMap.isKeyCaseSensitive)))
          }
          else
            Success(Some(wedgeValue))
        }
        else
          Success(Some(wedgeValue))
      case None =>
        Success(Some(wedgeValue))
    }

  private def tryOptionValueWedgeIsEmpty(wedgeKeyAbsolute: String, isValueEmptyString: Boolean): Try[Option[String]] =
    nexus.optionValueWedgeIsEmptyResolver match {
      case Some(valueWedgeIsEmptyResolver) =>
        Success(valueWedgeIsEmptyResolver(this, wedgeKeyAbsolute, isValueEmptyString))
      case None =>
        Success(
          if (isValueEmptyString)
            Some("")
          else
            None
        )
    }

  val valueTyped: ValueTyped =
    ValueTypedMap.tryApply(
        transformDateTimeStamped.transform.valueTypedMap.valueByKey
      , transformDateTimeStamped.transform.valueTypedMap.isKeyCaseSensitive
      , tryOptionValueWedgeNonEmpty
      , tryOptionValueWedgeIsEmpty
    ).get

  def tryCurrent: Try[Subset] =
    if (nexus.currentTransformDateTimeStamped.utcDateTimeStamp.isAfter(transformDateTimeStamped.utcDateTimeStamp))
      Subset.tryApply(nexus, keyPrefix, retainKeyPrefix)
    else
      Success(this)

  def toMap: Map[String, String] =
    transformDateTimeStamped.transform.valueTypedMap.valueByKey.keySet.map(
      key =>
        (key, valueTyped.Classes.Singles.tryOptionString(key).getOrElse(Some("<valueTyped.string(...) Failure>")).getOrElse("valueTyped.string(...) Success - None>"))
    ).toMap

  def trySubset(
      keyPrefix: String
    , retainKeyPrefix: Boolean = false
  ): Try[Subset] =
    Subset.tryApply(nexus, this.keyPrefix + keyPrefix, retainKeyPrefix)
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
