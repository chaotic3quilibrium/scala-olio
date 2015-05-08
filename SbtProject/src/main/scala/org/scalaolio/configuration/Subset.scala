package org.scalaolio.configuration

import scala.util.{Success, Try}

object Subset {
  def apply(
      nexus: Nexus
    , keyPrefix: String
    , retainKeyPrefix: Boolean = false
  ): Try[Subset] =
    validateAndConform(nexus, keyPrefix, retainKeyPrefix).flatMap(
      nexusAndKeyPrefixAndRetainKeyPrefix => {
        val transformDateTimeStamped =
          nexus.currentTransformDateTimeStamped
        transformDateTimeStamped.transform.subset(keyPrefix, retainKeyPrefix).flatMap(
          transformSubset =>
            Nexus.TransformDateTimeStamped(transformSubset, transformDateTimeStamped.utcDateTimeStamp).flatMap(
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
    nexus: Nexus
  , val keyPrefix: String
  , val retainKeyPrefix: Boolean
  , private val transformDateTimeStamped: Nexus.TransformDateTimeStamped //already filtered/truncated and non-empty
) {
  private def templateWedge(value: String): String =
    nexus.templateProfile match {
      case Some(templateProfile) =>
        if (value.nonEmpty && value.startsWith(templateProfile.prefix)) {
          val (valueTruncated, keys) =
            templateProfile.findKeys(value)
          if (keys.nonEmpty) {
            val keysResolved =
              if (transformDateTimeStamped.transform.valueTypedMap.isKeyCaseSensitive)
                keys
              else
                keys.map(_.toLowerCase)
            val keyAndValuesRelative =
              keysResolved.map(key => (key, transformDateTimeStamped.transform.valueTypedMap.valueByKey.get(key)))
            val keyAndValuesAbsolute =
              if (keyPrefix.nonEmpty && !retainKeyPrefix) {
                val unfoundKeys =
                  keyAndValuesRelative.filter(_._2.isEmpty).map(_._1)
                if (unfoundKeys.nonEmpty)
                  unfoundKeys.map(key => (key, nexus.currentTransformDateTimeStamped.transform.valueTypedMap.valueByKey.get(key)))
                else
                  Set()
              }
              else
                Set()
            val valueByKey = (
                 keyAndValuesRelative.filter(_._2.isDefined).map(knv => (knv._1, knv._2.get))
              ++ keyAndValuesAbsolute.filter(_._2.isDefined).map(knv => (knv._1, knv._2.get))
            ).toMap
            templateProfile.replace(value, valueByKey, transformDateTimeStamped.transform.valueTypedMap.isKeyCaseSensitive)
          }
          else
            value
        }
        else
          value
      case None =>
        value
    }

  val valueTyped: ValueTyped =
    ValueTypedMap(
        transformDateTimeStamped.transform.valueTypedMap.valueByKey
      , transformDateTimeStamped.transform.valueTypedMap.isKeyCaseSensitive
      , templateWedge
    ).get

  def current: Try[Subset] =
    if (nexus.currentTransformDateTimeStamped.utcDateTimeStamp.isAfter(transformDateTimeStamped.utcDateTimeStamp))
      Subset(nexus, keyPrefix, retainKeyPrefix)
    else
      Success(this)

  def subset(
      keyPrefix: String
    , retainKeyPrefix: Boolean = false
  ): Try[Subset] =
    Subset(nexus, this.keyPrefix + keyPrefix, retainKeyPrefix)
}
