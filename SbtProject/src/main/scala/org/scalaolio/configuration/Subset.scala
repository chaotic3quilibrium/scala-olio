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
  val valueTyped: ValueTyped =
    transformDateTimeStamped.transform.valueTypedMap

  val current: Try[Subset] =
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
