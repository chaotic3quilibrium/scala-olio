/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.java.lang.String_                         **
**   Name:      package.scala                                           **
**                                                                      **
** Description:                                                         **
**  Augments String with simple extensions                              **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.java.lang

import scala.annotation.tailrec
import scala.language.implicitConversions

package object String_ {
  def indexesOf(source: String, target: String, index: Int = 0, withinOverlaps: Boolean = false): List[Int] = {
    @tailrec
    def recursive(indexTarget: Int, accumulator: List[Int] = Nil): List[Int] = {
      val position = source.indexOf(target, indexTarget)
      if (position == -1) accumulator
      else
        recursive(position + (if (withinOverlaps) 1 else target.length), position :: accumulator)
    }
    recursive(index).reverse
  }

  def splitLiterally(string: String, separator: String): List[String] = {
    @tailrec
    def recursive(remaining: String, accumulator: List[String] = Nil): List[String] = {
      if (remaining.isEmpty)
        accumulator
      if (separator.isEmpty)
        remaining :: accumulator
      else {
        val index = remaining.indexOf(separator)
        if (index == -1)
          remaining :: accumulator
        else {
          recursive(remaining.drop(index + separator.length), remaining.take(index) :: accumulator)
        }
      }
    }
    recursive(string).reverse
  }

  def spanSansSeparator(string: String, separator: String): (String, String) = {
    if (separator.nonEmpty) {
      val index = string.indexOf(separator)
      if (index > -1)
        (string.take(index), string.drop(index + separator.length))
      else
        (string, "")
    }
    else
      (string, "")
  }

  implicit class RichString(val value: String) extends AnyVal {
    def indexesOf(target: String, index: Int = 0, withinOverlaps: Boolean = false): List[Int] =
      String_.indexesOf(value, target, index, withinOverlaps)

    def splitLiterally(separator: String): List[String] =
      String_.splitLiterally(value, separator)

    def spanSansSeparator(separator: String): (String, String) =
      String_.spanSansSeparator(value, separator)
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
