package org.scalaolio.configuration

import scala.util.{Failure, Success, Try}

import org.joda.time.{DateTime, DateTimeZone}

import org.scalaolio.collection.immutable.List_._
import org.scalaolio.java.lang.Class_._

object Nexus {
  object TransformNamed {
    def apply(
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
    def apply(
        transform: Transform
      , utcDateTimeStamp: DateTime
    ): Try[TransformDateTimeStamped] =
      Success(new TransformDateTimeStamped(transform, utcDateTimeStamp))
  }

  class TransformDateTimeStamped private[TransformDateTimeStamped] (
      val transform: Transform
    , val utcDateTimeStamp: DateTime
  )

  val defaultISODateTimeFormat = org.joda.time.format.ISODateTimeFormat.basicDateTime
  val TransformedNameRootName = "Nexus.TransformedName.Root"

  def apply(
      name: String
    , transformNamedNames: List[String]
  ): Try[Nexus] =
    validateAndConform(name, transformNamedNames).flatMap(
      nameAndTransformNamedNames =>
        Success(new Nexus(nameAndTransformNamedNames._1, nameAndTransformNamedNames._2))
    )

  def validateAndConform(
      name: String
    , transformNamedNames: List[String] //must be distinct, is case insensitive (all keys are internally forced to lower case) and is ordered least to most priority; i.e. for each item, it overrides what is left of it and is overridden by whatever, if anything exists to its right
  ): Try[(String, List[String])] =
    if (transformNamedNames.nonEmpty) {
      val (_, dupes) =
        transformNamedNames.map(_.toLowerCase).filterDupes
      if (dupes.isEmpty)
        Success((name, transformNamedNames))
      else
        Failure(new IllegalArgumentException(s"following the toLowerCase conversion, transformNamedNames must not contain duplicates [${dupes.mkString(",")}]"))
    }
    else
      Failure(new IllegalArgumentException("transformNamedNames must be nonEmpty"))
}

class Nexus private[Nexus] (
    val name: String
  , val transformNamedNames: List[String]
) {
  val utcCreated: DateTime =
    new DateTime(DateTimeZone.UTC)


  private val transformNamedNamesLowerCase =
    Nexus.TransformedNameRootName.toLowerCase :: transformNamedNames.map(_.toLowerCase)

  private val utcCreatedAndTransformNamedByTransformNamedNameLock = new Object //always synchronize on utcCreatedAndTransformNamedByTransformNamedNameLock (and not on utcCreatedAndTransformNamedByTransformNamedName)
  private var utcCreatedAndTransformNamedByTransformNamedName: Map[String, Option[(Nexus.TransformNamed, DateTime)]] = {
    def TransformedNameRoot: Nexus.TransformNamed = {
      val transform = {
        val prefix =
          getClass.fullName
        Transform(
          List(
              s"$prefix.name" -> name
            , s"$prefix.transformNamedNames" -> transformNamedNames.mkString(",")
            , s"$prefix.utcCreated" -> Nexus.defaultISODateTimeFormat.print(utcCreated)
          )
        ).get
      }
      Nexus.TransformNamed(transform, Nexus.TransformedNameRootName).get
    }
    (    (Nexus.TransformedNameRootName.toLowerCase, Some((TransformedNameRoot, utcCreated)))
      :: transformNamedNames.map(key => (key.toLowerCase, None))
    ).toMap
  }

  private val transformDateTimeStampedLock = new Object //always synchronize on transformDateTimeStampedLock (and not on transformDateTimeStamped)
  private var transformDateTimeStamped: Nexus.TransformDateTimeStamped =
    utcCreatedAndTransformNamedByTransformNamedNameLock.synchronized {
      utcCreatedAndTransformNamedByTransformNamedName(transformNamedNamesLowerCase.head) match {
        case Some(transformedNameAndDateTime) =>
          Nexus.TransformDateTimeStamped(transformedNameAndDateTime._1.transform, transformedNameAndDateTime._2).get
        case None =>
          throw new IllegalStateException("should not EVER get here, given utcCreatedAndTransformNamedByTransformNamedName properly initialized")
      }
    }

  private def isNameValid(name: String): Boolean = {
    val nameLowerCase =
      name.toLowerCase
    (nameLowerCase != transformNamedNamesLowerCase.head) && transformNamedNamesLowerCase.contains(nameLowerCase)
  }

  private def add(transformNamed: Nexus.TransformNamed, isMergeAndOverride: Boolean): Try[Nexus] = //can fail if a name is used which is equal to the value in TransformedNameRootName or does not exist in TransformNamedNamesOrderedFromLeastPriority
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
                Nexus.TransformNamed(transformNamed.transform.merge(transformedNamedAndDateTime._1.transform).get, transformNamed.name).get
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

  def addOrReplace(transformNamed: Nexus.TransformNamed): Try[Nexus] =
    add(transformNamed, isMergeAndOverride = false)

  def addOrMergeAndOverride(transformNamed: Nexus.TransformNamed): Try[Nexus] =
    add(transformNamed, isMergeAndOverride = true)

  def currentTransformDateTimeStamped: Nexus.TransformDateTimeStamped = {
    def compute(transformNamedAndDateTimes: List[(Nexus.TransformNamed, DateTime)]): Nexus.TransformDateTimeStamped = {
      def recursive(remaining: List[(Nexus.TransformNamed, DateTime)], accumulator: (Transform, DateTime)): (Transform, DateTime) = {
        if (remaining.isEmpty)
          accumulator
        else {
          val (transformNamed, dateTime) = remaining.head
          val transformNew =
            transformNamed.transform.merge(accumulator._1).get
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
      Nexus.TransformDateTimeStamped(temp._1, temp._2).get
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
}
