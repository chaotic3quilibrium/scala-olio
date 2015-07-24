/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util                                      **
**   Name:      SimpleCache.scala                                       **
**                                                                      **
** Description:                                                         **
**   Simplest implementation of a read-only key/value cache. There is   **
**   no means provided to change the value to which an existing key is  **
**   associated.                                                        **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2014 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util

/** Provides a very simple insert-only read-only cache implementation.
 *
 *  @tparam K The type used for the key.
 *  @tparam V The type used for the value.
 */
class SimpleCache [K, V] (
    content: Map[K, V] = Map[K, V]()
) {
  private val cacheLock: Object = new Object
  @volatile
  private var cache: Map[K, V] = content

  /** Obtain a read-only copy of the cache as a Map.
   *
   *  @return Copy of the cache as a Map
   */
  def snapshot: Map[K, V] = cache

  /** At key, obtain possible pre-existing value.
   *
   *  @param key Value to look up possible pre-existing value
   *  @return    Some(value) if a value has been stored previously,
   *             else None indicating the key does not have a value
   *             associated with it
   */
  def get(key: K): Option[V] = cache.get(key)

  /** Obtain possible pre-existing value. If key does not exist,
   *  generate and associate a value with it and place association in
   *  the cache.
   *
   *  @param key               Value to look up possible pre-existing
   *                           value, or to associate with the
   *                           generation of a new value
   *  @param elseValueProducer Function to generate an entry in cache,
   *                           if the key isn't already associated
   *                           Value associated with key
   */
  def getOrElse(
      key: K
    , elseValueProducer: K => V
  ): V = {
    def generateCacheEntry(key: K): V =
      cacheLock.synchronized {
        cache.get(key) match {
          case Some(value) =>
            value //prior thread populated the cache
          case None =>
            val value: V = elseValueProducer(key)
            cache += key -> value
            value
        }
      }
    cache.getOrElse(key, generateCacheEntry(key))
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
