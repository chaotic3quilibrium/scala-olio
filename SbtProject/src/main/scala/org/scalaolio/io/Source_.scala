/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.io                                        **
**   Name:      Source.scala                                            **
**                                                                      **
** Description:                                                         **
**  Substitutes exception and null prone Java classes and methods with  **
**  Scala artifacts (like Try and Option) which play much more          **
**  consistently with other Scala idioms                                **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2015 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.io

import scala.io.Source
import scala.util.Try

import java.io.File

object Source_ {
  def using[S <: Source, R](instantiateSource: () => S)(transfer: S => Try[R]): Try[R] =
    Try(instantiateSource()).flatMap(source => try transfer(source) finally source.close())

  def tryProcessByLine(
      file: File
    , parseLine: (Int, String) => Option[List[String]] =
        (index, unparsedLine) => Some(List(unparsedLine))
    , filterLine: (Int, List[String]) => Option[Boolean] =
        (index, parsedValues) => Some(true)
    , transform: (Int, List[String]) => Option[List[String]] =
        (index, parsedValues) => Some(parsedValues)
  ): Try[List[List[String]]] =
    tryProcessByLineBase(file, parseLine, filterLine, transform)

  def tryProcessByLineBase[A, B](
      file: File
    , parseLine: (Int, String) => Option[A]
    , filterLine: (Int, A) => Option[Boolean]
    , transform: (Int, A) => Option[B]
  ): Try[List[B]] = {
    def recursive(
      remaining: Iterator[(String, Int)],
      accumulator: List[B],
      isEarlyAbort: Boolean =
        false
    ): List[B] = {
      if (isEarlyAbort || !remaining.hasNext)
        accumulator
      else {
        val (line, index) =
          remaining.next
        parseLine(index, line) match {
          case Some(a) =>
            filterLine(index, a) match {
              case Some(keepA) =>
                if (keepA)
                  transform(index, a) match {
                    case Some(b) =>
                      recursive(remaining, b :: accumulator) //capture B
                    case None =>
                      recursive(remaining, accumulator, isEarlyAbort = true) //early abort
                  }
                else
                  recursive(remaining, accumulator) //discard A
              case None =>
                recursive(remaining, accumulator, isEarlyAbort = true) //early abort
            }
          case None =>
            recursive(remaining, accumulator, isEarlyAbort = true) //early abort
        }
      }
    }
    Try(Source.fromFile(file)).flatMap(
      bufferedSource =>
        using(() => bufferedSource) {
          source =>
            Try(recursive(source.getLines().buffered.zipWithIndex, Nil).reverse)
        }
    )
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
