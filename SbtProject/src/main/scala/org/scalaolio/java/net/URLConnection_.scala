/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.java.net                                  **
**   Name:      URLConnection_.scala                                    **
**                                                                      **
** Description:                                                         **
**  Substitutes exception and null prone Java classes and methods with  **
**  Scala artifacts (like Try and Option) which play much more          **
**  consistently with other Scala idioms                                **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2015 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.java.net

import scala.util.{Failure, Success, Try}

import java.io._
import java.net.{URI, URL, URLConnection}

import org.scalaolio.java.io.{BufferedReader_, Reader_}
import org.scalaolio.java.lang.AutoCloseable_

object URLConnection_ {
  val defaultBufferSize: Int =
    org.scalaolio.java.io.defaultBufferSize

  def readAsString(protocolPlusResourcePath: String): Try[String] =
    Try(new URL(protocolPlusResourcePath)) match {
      case Success(uRL) =>
        readAsString(uRL)
      case Failure(e) =>
        Failure(new IllegalStateException(s"unable to readAsString - " + "new URL(\"" + protocolPlusResourcePath + "\") failed - " + e.getMessage))
    }

  def readAsString(file: File): Try[String] =
    Try(file.toURI) match {
      case Success(uRI) =>
        readAsString(uRI)
      case Failure(e) =>
        Failure(new IllegalStateException(s"unable to readAsString - " + "file.toURI failed - " + e.getMessage))
    }

  def readAsString(uRI: URI): Try[String] =
    Try(uRI.toURL) match {
      case Success(uRL) =>
        readAsString(uRL)
      case Failure(e) =>
        Failure(new IllegalStateException(s"unable to readAsString - " + "uRI.toURL failed - " + e.getMessage))
    }

  def readAsString(uRL: URL): Try[String] =
    Try(uRL.openConnection) match {
      case Success(uRLConnection) =>
        Try(uRLConnection.connect()).flatMap(
          _ =>
            readAsString(uRLConnection)
        )
      case Failure(e) =>
        Failure(new IllegalStateException(s"unable to readAsString - " + "uRL.openConnection failed - " + e.getMessage))
    }

  def readAsString(
      uRLConnection: URLConnection
    , bufferSize: Int = defaultBufferSize
  ): Try[String] =
    AutoCloseable_.using(uRLConnection.getInputStream) {
      inputStream =>
        AutoCloseable_.using(() => new InputStreamReader(inputStream)) {
          inputStreamReader =>
            AutoCloseable_.using(() => new BufferedReader(inputStreamReader, bufferSize)) {
              bufferedReader =>
                Reader_.readAsString(bufferedReader, bufferSize)
            }
        }
    }

  def readAsListString(
      uRLConnection: URLConnection
    , bufferSize: Int = defaultBufferSize
  ): Try[List[String]] =
    AutoCloseable_.using(uRLConnection.getInputStream) {
      inputStream =>
        AutoCloseable_.using(() => new InputStreamReader(inputStream)) {
          inputStreamReader =>
            AutoCloseable_.using(() => new BufferedReader(inputStreamReader, bufferSize)) {
              bufferedReader =>
                BufferedReader_.readAsListString(bufferedReader)
            }
        }
    }

  def writeAsString(
      uRLConnection: URLConnection
    , content: String
    , bufferSize: Int = defaultBufferSize
  ): Try[Unit] =
    AutoCloseable_.using(uRLConnection.getOutputStream) {
      outputStream =>
        AutoCloseable_.using(() => new OutputStreamWriter(outputStream)) {
          outputStreamWriter =>
            AutoCloseable_.using(() => new BufferedWriter(outputStreamWriter, bufferSize)) {
              bufferedWriter => {
                val result =
                  Try(bufferedWriter.write(content))
                bufferedWriter.flush()
                result
              }
            }
        }
    }

  def writeAsListString(
      uRLConnection: URLConnection
    , lines: List[String]
    , bufferSize: Int = defaultBufferSize
  ): Try[Unit] =
    AutoCloseable_.using(uRLConnection.getOutputStream) {
      outputStream =>
        AutoCloseable_.using(() => new OutputStreamWriter(outputStream)) {
          outputStreamWriter =>
            AutoCloseable_.using(() => new BufferedWriter(outputStreamWriter, bufferSize)) {
              bufferedWriter =>
                AutoCloseable_.using(() => new PrintWriter(bufferedWriter)) {
                  printWriter => {
                    val result =
                      Try(lines.foreach(line => printWriter.println(line)))
                    printWriter.flush()
                    result
                  }
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
