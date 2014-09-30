/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.java.io.File_                             **
**   Name:      package.scala                                           **
**                                                                      **
** Description:                                                         **
**  Implements the ARM (Automatic Resource Management) pattern          **
**  implemented in Java 7 (1.7). Substitutes exception and null prone   **
**  Java classes and methods with Scala artifacts (like Try and Option) **
**  which play much more consistently with other Scala idioms           **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2014 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.java.io

import java.io._

import scala.util.Try

import org.scalaolio.java.lang.AutoCloseable_._
import org.scalaolio.java.Io_
import org.scalaolio.java.Io_._
import org.scalaolio.util.Try_.CompletedNoException

/** Encapsulate with idiomatic Scala all the Java null and checked
 *  exception prone methods in Java.
 */
package object File_ {
  /** Byte based input/output methods
   */
  object ArrayByteBased {
    /** Using ARM, pulls the content through an BufferedInputStream out
     *  of an InputStream which is generated from a File which is
     *  instantiated from a path and file String.
     *
     *  @param fileAsString File based resource specified as a String
     *  @return             Either Success(Array[Byte]) which holds the
     *                      retrieved content, or Failure(Exception)
     *                      where the Exception was captured by a
     *                      try/catch block
     */
    def fromInputStream(fileAsString: String): Try[Array[Byte]] =
      Try(new File(fileAsString)).flatMap(
        (file) => fromInputStream(file)
      )

    /** Using ARM, pushes the content through an BufferedOutputStream
     *  into an OutputStream which is generated from a File which is
     *  instantiated from a path and file String.
     *
     *  @param fileAsString File based resource specified as a String
     *  @param content      Information to persist
     *  @return             Either
     *                      Success(completedNoExceptionSingleton) or
     *                      Failure(Exception) where the Exception was
     *                      captured by a try/catch block
     */
    def toOutputStream(
        fileAsString: String
      , content: Array[Byte]
    ): Try[CompletedNoException] =
      Try(new File(fileAsString)).flatMap(
        (file) => toOutputStream(file, content)
      )

    /** Using ARM, pulls the content through an BufferedInputStream out
     *  of an InputStream which is generated from a File.
     *
     *  @param file       File based resource
     *  @param bufferSize Optionally override the default buffer size
     *  @return           Either Success(Array[Byte]) which holds the
     *                    retrieved content, or Failure(Exception)
     *                    where the Exception was captured by a
     *                    try/catch block
     */
    def fromInputStream(
        file: File
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[Array[Byte]] =
      operate[FileInputStream, Array[Byte]](
          () => new FileInputStream(file)
        , (fileInputStream) =>
            Io_.ArrayByteBased.fromInputStream(
                fileInputStream
              , bufferSize
            )
      )

    /** Using ARM, pushes the content through an BufferedOutputStream
     *  into an OutputStream which is generated from a File.
     *
     *  @param file       File based resource
     *  @param content    Information to persist
     *  @param bufferSize Optionally override the default buffer size
     *  @return           Either
     *                    Success(completedNoExceptionSingleton) or
     *                    Failure(Exception) where the Exception was
     *                    captured by a try/catch block
     */
    def toOutputStream(
        file: File
      , content: Array[Byte]
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[CompletedNoException] =
      operate[FileOutputStream, CompletedNoException](
          () => new FileOutputStream(file)
        , (fileOutputStream) =>
            Io_.ArrayByteBased.toOutputStream(
                fileOutputStream
              , content
              , bufferSize
            )
      )
  }

  /** String based input/output methods
   */
  object StringBased {
    /** Using ARM, pulls the content through a BufferedReader out of a
     *  Reader which is generated from a File which is instantiated
     *  from a path and file String.
     *
     *  @param fileAsString File based resource specified as a String
     *  @return             Either Success(String) which holds the
     *                      retrieved content, or Failure(Exception)
     *                      where the Exception was captured by a
     *                      try/catch block
     */
    def fromReader(fileAsString: String): Try[String] =
      Try(new File(fileAsString)).flatMap(
        (file) => fromReader(file)
      )

    /** Using ARM, pushes the content as a String through BufferedWriter
     *  into a Writer which is generated from a File which is
     *  instantiated from a path and file String.
     *
     *  @param fileAsString File based resource specified as a String
     *  @param content      Information to persist
     *  @return             Either
     *                      Success(completedNoExceptionSingleton) or
     *                      Failure(Exception) where the Exception was
     *                      captured by a try/catch block
     */
    def toPrintWriter(
        fileAsString: String
      , content: String
    ): Try[CompletedNoException] =
      Try(new File(fileAsString)).flatMap(
        (file) => toPrintWriter(file, content)
      )

    /** Using ARM, pulls the content through a BufferedReader out of a
     *  Reader which is generated from a File.
     *
     *  @param file       File based resource
     *  @param bufferSize Optionally override the default buffer size
     *  @return           Either Success(String) which holds the
     *                    retrieved content, or Failure(Exception)
     *                    where the Exception was captured by a
     *                    try/catch block
     */
    def fromReader(
        file: File
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[String] =
      operate[FileReader, String](
          () => new FileReader(file)
        , (fileReader) =>
            Io_.StringBased.fromReader(fileReader, bufferSize)
      )

    /** Using ARM, pushes the content as a String through BufferedWriter
     *  into a Writer which is generated from a File.
     *
     *  @param file       File based resource
     *  @param content    Information to persist
     *  @param bufferSize Optionally override the default buffer size
     *  @return           Either
     *                    Success(completedNoExceptionSingleton) or
     *                    Failure(Exception) where the Exception was
     *                    captured by a try/catch block
     */
    def toPrintWriter(
        file: File
      , content: String
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[CompletedNoException] =
      operate[PrintWriter, CompletedNoException](
          () => new PrintWriter(file)
        , (printWriter) =>
          Io_.StringBased.toWriter(printWriter, content, bufferSize)
      )
  }

  /** List[String] based input/output methods
   */
  object ListStringBased {
    /** Using ARM, pulls the content through a BufferedReader out of a
     *  Reader which is generated from a File which is instantiated
     *  from a path and file String.
     *
     *  @param fileAsString File based resource specified as a String
     *  @return             Either Success(List[String]) which holds
     *                      the retrieved content, or
     *                      Failure(Exception) where the Exception was
     *                      captured by a try/catch block
     */
    def fromReader(fileAsString: String): Try[List[String]] =
      Try(new File(fileAsString)).flatMap(
        (file) => fromReader(file)
      )

    /** Using ARM, pushes the content as a String through BufferedWriter
     *  into a Writer which is generated from a File which is
     *  instantiated from a path and file String.
     *
     *  @param fileAsString File based resource specified as a String
     *  @param content      Information to persist
     *  @return             Either
     *                      Success(completedNoExceptionSingleton) or
     *                      Failure(Exception) where the Exception was
     *                      captured by a try/catch block
     */
    def toPrintWriter(
        fileAsString: String
      , content: List[String]
    ): Try[CompletedNoException] =
      Try(new File(fileAsString)).flatMap(
        (file) => toPrintWriter(file, content)
      )

    /** Using ARM, pulls the content through a BufferedReader out of a
     *  Reader which is generated from a File.
     *
     *  @param file       File based resource
     *  @param bufferSize Optionally override the default buffer size
     *  @return           Either Success(List[String]) which holds
     *                    the retrieved content, or
     *                    Failure(Exception) where the Exception was
     *                    captured by a try/catch block
     */
    def fromReader(
        file: File
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[List[String]] =
      operate[FileReader, List[String]](
          () => new FileReader(file)
        , (fileReader) =>
            Io_.ListStringBased.fromReader(fileReader, bufferSize)
      )

    /** Using ARM, pushes the content as a String through BufferedWriter
     *  into a Writer which is generated from a File.
     *
     *  @param file       File based resource
     *  @param content    Information to persist
     *  @param bufferSize Optionally override the default buffer size
     *  @return           Either
     *                    Success(completedNoExceptionSingleton) or
     *                    Failure(Exception) where the Exception was
     *                    captured by a try/catch block
     */
    def toPrintWriter(
        file: File
      , content: List[String]
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[CompletedNoException] =
      operate[PrintWriter, CompletedNoException](
          () => new PrintWriter(file)
        , (printWriter) =>
            Io_.ListStringBased.toWriter(
                printWriter
              , content
              , bufferSize
            )
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
