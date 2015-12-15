/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.java.io                                   **
**   Name:      Reader_.scala                                           **
**                                                                      **
** Description:                                                         **
**  Substitutes exception and null prone Java classes and methods with  **
**  Scala artifacts (like Try and Option) which play much more          **
**  consistently with other Scala idioms                                **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2015 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.java.io

import scala.annotation.tailrec
import scala.util.{Success, Try}

import java.io.Reader

object Reader_ {
  val defaultBufferSize: Int =
    org.scalaolio.java.io.defaultBufferSize

  def readAsString(
      from: Reader
    , bufferSize: Int = defaultBufferSize
  ): Try[String] = {
    //read data out of stream
    @tailrec
    def recursive(
        accumulator: List[(Array[Char], Int)]
    ): List[(Array[Char], Int)] = {
      val buffer = new Array[Char](bufferSize)
      //- from.read can fetch less than
      //  bufferSize bytes and still not be at the end of the
      //  file
      val charsRead = from.read(buffer)
      if (charsRead < 1) accumulator
      else recursive((buffer, charsRead) :: accumulator)
    }
    // Concatenate a sequence of byte buffers into a single one.
    def flatten(
        sliceAndSize: List[(Array[Char], Int)]
    ): Array[Char] =
      if (sliceAndSize.nonEmpty) {
        val bufferSize = sliceAndSize.map(_._2).sum
        val buffer = new Array[Char](bufferSize)
        var bufferPosition = 0
        sliceAndSize.foreach(charsAndSize => {
          Array.copy(
              charsAndSize._1
            , 0
            , buffer
            , bufferPosition
            , charsAndSize._2
          )
          bufferPosition += charsAndSize._2
        })
        buffer
      }
      else
        Array.emptyCharArray
    Success(flatten(recursive(Nil).reverse).mkString)
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
