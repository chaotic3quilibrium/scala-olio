/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util                                      **
**   Name:      TimeWindow.scala                                        **
**                                                                      **
** Description:                                                         **
**   Implements a full time window. It has an inclusive starting time,  **
**   and optional exclusive expiration time. And if the expiration time **
**   is supplied, an optional exclusive deprecation time may be         **
**   provided. All times are UTC. Any need for a non-UTC DateTime is    **
**   considered a "rendering" issue.                                    **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2014 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util

import org.joda.time.DateTime
import org.joda.time.DateTimeZone

object TimeWindow {
  object EffectiveFactory {
    private val simpleCache =
      new SimpleCache[(DateTime, Option[Expire]), Effective]
    def apply(
        startUtc: DateTime = new DateTime(DateTimeZone.UTC)
      , expire: Option[Expire] = None
    ): Effective =
      simpleCache.getOrElse(
          (startUtc, expire)
        , k => Effective.tupled(k)
      )
  }

  case class Effective private [TimeWindow] (
      startUtc: DateTime
    , expire: Option[Expire]
  ) {
    require(
        startUtc.getZone == DateTimeZone.UTC
      , "startUtc.getZone [" + startUtc.getZone + "] must be equal to DateTimeZone.UTC [" + DateTimeZone.UTC + "]"
    )
    require(
        expire.isEmpty || startUtc.isBefore(expire.get.startUtc)
      , "expire must be None, or startUtc [" + startUtc + "] must be before expire.get.whenUtc[" + expire.get.startUtc + "]"
    )
    require(
        expire.isEmpty || expire.get.deprecateUtc.isEmpty || startUtc.isBefore(expire.get.deprecateUtc.get)
        , "expire must be None, or expire.get.deprecateUtc must be None, or startUtc [" + startUtc + "] must be before expire.get.deprecateUtc.get[" + expire.get.deprecateUtc.get + "]"
    )
    def copy(
      startUtc: DateTime = this.startUtc
    , expiration: Option[Expire] = this.expire
    ): Effective = EffectiveFactory(startUtc, expiration)
    def isOverlapping(that: Effective): Boolean = (
           (this == that)                                             //same instance
        || (startUtc == that.startUtc)                                //same start time
        || {
             val (earlier, later) =
               if (startUtc.isBefore(that.startUtc)) (this, that)
               else (that, this)
             (
                  earlier.expire.isEmpty                              //begins overlapping at later.effectiveUtc
               || earlier.expire.get.startUtc.isAfter(later.startUtc) //begins overlapping at later.effectiveUtc
             )
           }
    )
  }

  object ExpireFactory {
    private val simpleCache  =
      new SimpleCache[(DateTime, Option[DateTime]), Expire]
    def apply(
        startUtc: DateTime
      , deprecateUtc: Option[DateTime] = None
    ): Expire =
      simpleCache.getOrElse(
          (startUtc, deprecateUtc)
        , k => Expire.tupled(k)
      )
  }

  case class Expire private[TimeWindow] (
      startUtc: DateTime
    , deprecateUtc: Option[DateTime]
  ) {
    require(
        startUtc.getZone == DateTimeZone.UTC
      , "startUtc.getZone [" + startUtc.getZone + "] must be equal to DateTimeZone.UTC [" + DateTimeZone.UTC + "]"
    )
    require(
        deprecateUtc.isEmpty || (deprecateUtc.get.getZone == DateTimeZone.UTC)
      , "deprecateUtc must be None, or deprecateUtc.get.getZone [" + deprecateUtc.get.getZone + "] must be equal to DateTimeZone.UTC [" + DateTimeZone.UTC + "]"
    )
    require(
        deprecateUtc.isEmpty || startUtc.isAfter(deprecateUtc.get)
      , "deprecateUtc must be None, or startUtc [" + startUtc + "] must be after deprecateUtc.get [" + deprecateUtc.get + "]"
    )
    def copy(
        startUtc: DateTime = this.startUtc
      , deprecateUtc: Option[DateTime] = this.deprecateUtc
    ): Expire = ExpireFactory(startUtc, deprecateUtc)
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
