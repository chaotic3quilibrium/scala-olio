/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.configuration                             **
**   Name:      Nexus.scala                                             **
**                                                                      **
** Description:                                                         **
**  Root of configuration factory and takes multiple sub-configurations **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.configuration

import scala.util.{Failure, Success, Try}

import org.joda.time.{DateTime, DateTimeZone}

import org.scalaolio.collection.immutable.List_._
import org.scalaolio.java.lang.Class_._
import org.scalaolio.java.lang.String_._

object Nexus {
  object TransformNamed {
    def tryApply(
        transform: Transform
      , name: String
      ): Try[TransformNamed] =
        Success(new TransformNamed(transform, name))
  }

  class TransformNamed private[TransformNamed] (
      val transform: Transform
    , val name: String
  )

  object TransformDateTimeStamped {
    def tryApply(
        transform: Transform
      , utcDateTimeStamp: DateTime
    ): Try[TransformDateTimeStamped] =
      Success(new TransformDateTimeStamped(transform, utcDateTimeStamp))
  }

  class TransformDateTimeStamped private[TransformDateTimeStamped] (
      val transform: Transform
    , val utcDateTimeStamp: DateTime
  )

  object TemplateProfile {
    val DEFAULT_PREFIX = "template=>"
    val DEFAULT_OPEN = "["
    val DEFAULT_CLOSE = "]"

    def default: TemplateProfile =
      TemplateProfile.tryApply().get

    def tryApply(
        prefix: String = DEFAULT_PREFIX
      , open: String = DEFAULT_OPEN
      , close: String = DEFAULT_CLOSE
    ): Try[TemplateProfile] =
      if (prefix.nonEmpty)
        if (open.nonEmpty)
          if (close.nonEmpty)
            if (!open.contains(close))
              if (!close.contains(open))
                Success(new TemplateProfile(prefix, open, close))
              else
                Failure(new IllegalArgumentException(s"open [$open] must not be equal to or contained by close [$close]"))
            else
              Failure(new IllegalArgumentException(s"close [$close] must not be equal to or contained by open [$open]"))
          else
            Failure(new IllegalArgumentException("close must be nonEmpty"))
        else
          Failure(new IllegalArgumentException("open must be nonEmpty"))
      else
        Failure(new IllegalArgumentException("prefix must be nonEmpty"))
  }

  class TemplateProfile private[TemplateProfile] (
      val prefix: String
    , val open: String
    , val close: String
  ) {
    def findOpenAndCloses(string: String): (String, List[(Int, Int)]) = //(truncatedString, List((indexOfOpen, indexOfClose)))
      if (string.nonEmpty && string.startsWith(prefix)) {
        val truncated =
          string.drop(prefix.length)
        val opens =
          truncated.indexesOf(open)
        val closes =
          truncated.indexesOf(close)
        if (opens.size == closes.size) {
          val openAndCloses =
            opens.zip(closes)
          if (!openAndCloses.exists(onc => onc._1 > onc._2)) //equals should be impossible if preconditions in validate hold
            (truncated, openAndCloses)
          else
            (truncated, Nil) //at some point an open followed its close
        }
        else
          (truncated, Nil) //not an equal number of opens to closes
      }
      else
        (string, Nil)

    def findKeys(string: String): (String, Set[String]) = { //sans open and close
      val (stringNew, opensAndCloses) =
        findOpenAndCloses(string)
      if (opensAndCloses.nonEmpty)
        (stringNew, opensAndCloses.map(onc => stringNew.substring(onc._1 + open.length, onc._2)).toSet)
      else
        (stringNew, Set())
    }

    def replace(string: String, valueByKey: Map[String, String], isKeyCaseSensitive: Boolean): String = {
      val (stringNew, opensAndCloses) =
        findOpenAndCloses(string)
      if (opensAndCloses.nonEmpty) {
        val keeping =
          (0 :: opensAndCloses.map(onc => List(onc._1, onc._2 + close.length)).flatten ::: List(stringNew.length)).grouped(2).map(ssbe => stringNew.substring(ssbe.head, ssbe.tail.head))
        val replacements =
          opensAndCloses.map(onc => stringNew.substring(onc._1 + open.length, onc._2)).map(key => valueByKey.getOrElse(if (isKeyCaseSensitive) key else key.toLowerCase, open + key + close)) ::: List("")
        keeping.toList.zip(replacements).map(knr => List(knr._1, knr._2)).flatten.mkString
      }
      else
        stringNew
    }
  }

  val defaultISODateTimeFormat = org.joda.time.format.ISODateTimeFormat.basicDateTime
  val transformedNameRootName = "Nexus.TransformedName.Root"

  def tryApply(
      name: String
    , transformNamedNames: List[String]
    , templateProfile: Option[TemplateProfile] = Some(TemplateProfile.default)
    , optionValueWedgeNonEmptyTemplateExceptionResolver: Option[(Subset, String, String, String) => Option[String]] = None //Some((subset, wedgeKeyAbsolute, templateKey, wedgeValueSanTemplatePrefix))
    , optionValueWedgeIsEmptyResolver: Option[(Subset, String, Boolean) => Option[String]] = None
  ): Try[Nexus] =
    tryValidateAndConform(name, transformNamedNames, templateProfile).flatMap(
      nameTransformNamedNamesAndTemplateProfile =>
        Success(new Nexus(nameTransformNamedNamesAndTemplateProfile._1, nameTransformNamedNamesAndTemplateProfile._2, nameTransformNamedNamesAndTemplateProfile._3, optionValueWedgeNonEmptyTemplateExceptionResolver, optionValueWedgeIsEmptyResolver))
    )

  def tryValidateAndConform(
      name: String
    , transformNamedNames: List[String] //must be distinct, is case insensitive (all keys are internally forced to lower case) and is ordered least to most priority; i.e. for each item, it overrides what is left of it and is overridden by whatever, if anything exists to its right
    , templateProfile: Option[TemplateProfile]
  ): Try[(String, List[String], Option[TemplateProfile])] =
    if (transformNamedNames.nonEmpty) {
      val (_, dupes) =
        transformNamedNames.map(_.toLowerCase).filterDupes
      if (dupes.isEmpty)
        Success((name, transformNamedNames, templateProfile))
      else
        Failure(new IllegalArgumentException(s"following the toLowerCase conversion, transformNamedNames must not contain duplicates [${dupes.mkString(",")}]"))
    }
    else
      Failure(new IllegalArgumentException("transformNamedNames must be nonEmpty"))
}

class Nexus private[Nexus] (
    val name: String
  , val transformNamedNames: List[String]
  , val templateProfile: Option[Nexus.TemplateProfile]
  , val optionValueWedgeNonEmptyTemplateExceptionResolver: Option[(Subset, String, String, String) => Option[String]]
  , val optionValueWedgeIsEmptyResolver: Option[(Subset, String, Boolean) => Option[String]]
) {
  val utcCreated: DateTime =
    new DateTime(DateTimeZone.UTC)

  private val transformNamedNamesLowerCase =
    Nexus.transformedNameRootName.toLowerCase :: transformNamedNames.map(_.toLowerCase)

  private val utcCreatedAndTransformNamedByTransformNamedNameLock = new Object //always synchronize on utcCreatedAndTransformNamedByTransformNamedNameLock (and not on utcCreatedAndTransformNamedByTransformNamedName)
  private var utcCreatedAndTransformNamedByTransformNamedName: Map[String, Option[(Nexus.TransformNamed, DateTime)]] = {
    def transformedNameRoot: Nexus.TransformNamed = {
      val transform = {
        val prefix =
          getClass.fullName
        Transform(
          ValueTypedMap.tryApply(
            Map(
                s"$prefix.name" -> name
              , s"$prefix.transformNamedNames" -> transformNamedNames.mkString(",")
              , s"$prefix.utcCreated" -> Nexus.defaultISODateTimeFormat.print(utcCreated)
            )
          ).get
        )
      }
      Nexus.TransformNamed.tryApply(transform, Nexus.transformedNameRootName).get
    }
    (    (Nexus.transformedNameRootName.toLowerCase, Some((transformedNameRoot, utcCreated)))
      :: transformNamedNames.map(key => (key.toLowerCase, None))
    ).toMap
  }

  private val transformDateTimeStampedLock = new Object //always synchronize on transformDateTimeStampedLock (and not on transformDateTimeStamped)
  private var transformDateTimeStamped: Nexus.TransformDateTimeStamped =
    utcCreatedAndTransformNamedByTransformNamedNameLock.synchronized {
      utcCreatedAndTransformNamedByTransformNamedName(transformNamedNamesLowerCase.head) match {
        case Some(transformedNameAndDateTime) =>
          Nexus.TransformDateTimeStamped.tryApply(transformedNameAndDateTime._1.transform, transformedNameAndDateTime._2).get
        case None =>
          throw new IllegalStateException("should not EVER get here, given utcCreatedAndTransformNamedByTransformNamedName properly initialized")
      }
    }

  private def isNameValid(name: String): Boolean = {
    val nameLowerCase =
      name.toLowerCase
    (nameLowerCase != transformNamedNamesLowerCase.head) && transformNamedNamesLowerCase.contains(nameLowerCase)
  }

  private def tryAdd(transformNamed: Nexus.TransformNamed, isMergeAndOverride: Boolean): Try[Nexus] = //can fail if a name is used which is equal to the value in TransformedNameRootName or does not exist in TransformNamedNamesOrderedFromLeastPriority
    if (isNameValid(transformNamed.name))
      utcCreatedAndTransformNamedByTransformNamedNameLock.synchronized {
        val utcDateTime = {
          val dateTimeTemp =
            new DateTime(DateTimeZone.UTC)
          if (dateTimeTemp.isAfter(transformDateTimeStamped.utcDateTimeStamp))
            dateTimeTemp
          else
            transformDateTimeStamped.utcDateTimeStamp.plusMillis(1) //if the system is so damn fast not even a millisecond could pass between the last transformAsNow update, then force the DateTime stamp forward in time just beyond the latest time updated
        }
        val transformNamedNew: Nexus.TransformNamed =
          if (isMergeAndOverride)
            utcCreatedAndTransformNamedByTransformNamedName(transformNamed.name.toLowerCase) match {
              case Some(transformedNamedAndDateTime) =>
                Nexus.TransformNamed.tryApply(transformNamed.transform.tryMerge(transformedNamedAndDateTime._1.transform).get, transformNamed.name).get
              case None =>
                transformNamed
            }
          else
            transformNamed
        utcCreatedAndTransformNamedByTransformNamedName =
          utcCreatedAndTransformNamedByTransformNamedName + (transformNamed.name.toLowerCase -> Some((transformNamedNew, utcDateTime)))
        Success(this)
      }
    else
      Failure(new IllegalArgumentException(s"transformNamed.name [${transformNamed.name}] must be one of transformNamedNames [${transformNamedNames.mkString(",")}]"))

  def tryAddOrReplace(transformNamed: Nexus.TransformNamed): Try[Nexus] =
    tryAdd(transformNamed, isMergeAndOverride = false)

  def tryAddOrMergeAndOverride(transformNamed: Nexus.TransformNamed): Try[Nexus] =
    tryAdd(transformNamed, isMergeAndOverride = true)

  def currentTransformDateTimeStamped: Nexus.TransformDateTimeStamped = {
    def compute(transformNamedAndDateTimes: List[(Nexus.TransformNamed, DateTime)]): Nexus.TransformDateTimeStamped = {
      def recursive(remaining: List[(Nexus.TransformNamed, DateTime)], accumulator: (Transform, DateTime)): (Transform, DateTime) = {
        if (remaining.isEmpty)
          accumulator
        else {
          val (transformNamed, dateTime) = remaining.head
          val transformNew =
            transformNamed.transform.tryMerge(accumulator._1).get
          val dateTimeNew =
            if (dateTime.isAfter(accumulator._2))
              dateTime
            else
              accumulator._2
          recursive(remaining.tail, (transformNew, dateTimeNew))
        }
      }
      val temp =
        recursive(transformNamedAndDateTimes.tail, (transformNamedAndDateTimes.head._1.transform, transformNamedAndDateTimes.head._2))
      Nexus.TransformDateTimeStamped.tryApply(temp._1, temp._2).get
    }
    var mapDefined: Map[String, Option[(Nexus.TransformNamed, DateTime)]] =
      Map()
    transformDateTimeStampedLock.synchronized {
      utcCreatedAndTransformNamedByTransformNamedNameLock.synchronized {
        mapDefined =
          utcCreatedAndTransformNamedByTransformNamedName.filter(_._2.isDefined)
      }
      val mapFlattened =
        mapDefined.map(x => (x._1, x._2.get))
      if (mapFlattened.nonEmpty) {
        if (mapFlattened.exists(x => x._2._2.isAfter(transformDateTimeStamped.utcDateTimeStamp))) {
          //iterates though versions merging and overriding from least to highest priority
          val list =
            transformNamedNamesLowerCase.filter(key => mapFlattened.keySet.contains(key)).map(key => mapFlattened(key))
          transformDateTimeStamped =
            compute(list)
        }
        transformDateTimeStamped
      }
      else
        throw new IllegalStateException("should not EVER get here, given utcCreatedAndTransformNamedByTransformNamedName properly initialized")
    }
  }

  def trySubset(
      keyPrefix: String
    , retainKeyPrefix: Boolean = false
  ): Try[Subset] =
    Subset.tryApply(this, keyPrefix, retainKeyPrefix)
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
