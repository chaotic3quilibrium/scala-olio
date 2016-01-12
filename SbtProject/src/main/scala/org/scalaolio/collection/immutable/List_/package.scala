/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.collection.immutable.List_                **
**   Name:      package.scala                                           **
**                                                                      **
** Description:                                                         **
**  Augments List with simple extensions                                **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.collection.immutable

package object List_ {
  def filterDupes[A](items: List[A]): (List[A], List[(A, Int)]) = {
    def recursive(remaining: List[A], index: Int, accumulator: (List[A], List[(A, Int)])): (List[A], List[(A, Int)]) = {
      if (remaining.isEmpty)
        accumulator
      else
        recursive(
            remaining.tail
          , index + 1
          , if (accumulator._1.contains(remaining.head))
              (accumulator._1, (remaining.head, index) :: accumulator._2)
            else
              (remaining.head :: accumulator._1, accumulator._2)
        )
    }
    val (distinct, dupes) = recursive(items, 0, (Nil, Nil))
    (distinct.reverse, dupes.reverse)
  }

  implicit class RichList[A](val value: List[A]) extends AnyVal {
    def filterDupes: (List[A], List[(A, Int)]) =
      List_.filterDupes(value)
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
