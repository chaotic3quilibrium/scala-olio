/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.io                                        **
**   Name:      File_.scala                                             **
**                                                                      **
** Description:                                                         **
**  Substitutes exception and null prone Java classes and methods with  **
**  Scala artifacts (like Try and Option) which play much more          **
**  consistently with other Scala idioms                                **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.java.io

import scala.util.Try

import java.io._

import org.scalaolio.java.lang.AutoCloseable_

object File_ {
  val defaultBufferSize: Int =
    org.scalaolio.java.io.defaultBufferSize

  def readAsString(
      location: File
    , bufferSize: Int = defaultBufferSize
  ): Try[String] =
    AutoCloseable_.using(() => new FileReader(location)) {
      fileReader =>
        AutoCloseable_.using(() => new BufferedReader(fileReader, bufferSize)) {
          bufferedReader =>
            Reader_.readAsString(bufferedReader, bufferSize)
        }
    }

  def readAsListString(
      location: File
    , bufferSize: Int = defaultBufferSize
  ): Try[List[String]] =
    AutoCloseable_.using(() => new FileReader(location)) {
      fileReader =>
        AutoCloseable_.using(() => new BufferedReader(fileReader, bufferSize)) {
          bufferedReader =>
            BufferedReader_.readAsListString(bufferedReader)
        }
    }

  def writeAsString(
      location: File
    , content: String
    , bufferSize: Int = defaultBufferSize
  ): Try[Unit] = {
    AutoCloseable_.using(() => new FileWriter(location)) {
      fileWriter =>
        AutoCloseable_.using(() => new BufferedWriter(fileWriter, bufferSize)) {
          bufferedWriter =>
            Try(bufferedWriter.write(content))
        }
    }
  }

  def writeAsListString(
      location: File
    , lines: List[String]
    , bufferSize: Int = defaultBufferSize
  ): Try[Unit] = {
    AutoCloseable_.using(() => new FileWriter(location)) {
      fileWriter =>
        AutoCloseable_.using(() => new BufferedWriter(fileWriter, bufferSize)) {
          bufferedWriter =>
            AutoCloseable_.using(() => new PrintWriter(bufferedWriter)) {
              printWriter =>
                Try(lines.foreach(line => printWriter.println(line)))
            }
        }
    }
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
