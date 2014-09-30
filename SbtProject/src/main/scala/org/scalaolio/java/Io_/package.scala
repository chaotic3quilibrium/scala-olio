/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.java.Io_                                  **
**   Name:      package.scala                                           **
**                                                                      **
** Description:                                                         **
**  Substitutes exception and null prone Java classes and methods with  **
**  Scala artifacts (like Try and Option) which play much more          **
**  consistently with other Scala idioms                                **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2014 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.java

import java.io._

import scala.annotation.tailrec
import scala.util.{Success, Try}

import org.scalaolio.java.lang.AutoCloseable_._
import org.scalaolio.util.Try_._

/** Encapsulate with idiomatic Scala all the Java null and checked
 *  exception prone methods in Java.
 */
package object Io_ {
  /** Provides a more sensible default than the one in java.io (which
   *  vary from 2048 to 8192)
   */
  val DEFAULT_BUFFER_SIZE = 65536

  /** Byte based input/output methods
   */
  object ArrayByteBased {
    /** Concatenate a sequence of byte buffers into a single one.
     *
     *  @param sliceAndSize A list of byte buffers with their size
     *  @return             Single byte buffer consisting of a
     *                      concatenation of the list of byte buffers
     */
    private def flatten(
        sliceAndSize: List[(Array[Byte], Int)]
    ): Array[Byte] =
      if (sliceAndSize.nonEmpty) {
        val bufferSize = sliceAndSize.map(_._2).sum
        val buffer = new Array[Byte](bufferSize)
        var bufferPosition = 0
        sliceAndSize.foreach(bytesAndSize => {
          Array.copy(
              bytesAndSize._1
            , 0
            , buffer
            , bufferPosition
            , bytesAndSize._2
          )
          bufferPosition += bytesAndSize._2
        })
        buffer
      }
      else
        Array.emptyByteArray

    /** Using ARM, pulls the content through an BufferedInputStream out
     *  of InputStream.
     *
     *  @param inputStream The resource which is to be wrapped by an
     *                     BufferedInputStream and through which the
     *                     content is pulled
     *  @param bufferSize  Optionally override the default buffer size
     *  @return            Either Success(Array[Byte]) which holds the
     *                     retrieved content, or Failure(Exception)
     *                     where the Exception was captured by a
     *                     try/catch block
     */
    def fromInputStream(
        inputStream: InputStream
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[Array[Byte]] =
      operate[BufferedInputStream, Array[Byte]](
          () => new BufferedInputStream(inputStream, bufferSize)
        , (bufferedInputStream) => {
            @tailrec
            def recursive(
                accumulator: List[(Array[Byte], Int)]
            ): List[(Array[Byte], Int)] = {
              val buffer = new Array[Byte](bufferSize)
              //- bufferedInputStream.read can fetch less than
              //  bufferSize bytes and still not be at the end of the
              //  file
              val bytesRead = bufferedInputStream.read(buffer)
              if (bytesRead < 1) accumulator
              else recursive((buffer, bytesRead) :: accumulator)
            }
            Success(flatten(recursive(Nil).reverse))
          }
    )

    /** Using ARM, pushes the content through an BufferedOutputStream
     *  into an OutputStream.
     *
     *  @param outputStream The resource which is to be wrapped by an
     *                      BufferedOutputStream and through which the
     *                      content is pushed
     *  @param content      Information to persist
     *  @param bufferSize   Optionally override the default buffer size
     *  @return             Either
     *                      Success(completedNoExceptionSingleton) or
     *                      Failure(Exception) where the Exception was
     *                      captured by a try/catch block
     */
    def toOutputStream(
        outputStream: OutputStream
      , content: Array[Byte]
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[CompletedNoException] =
      operate[BufferedOutputStream, CompletedNoException](
          () => new BufferedOutputStream(outputStream, bufferSize)
        , (bufferedOutputStream) => {
            bufferedOutputStream.write(content)
            Success(completedNoExceptionSingleton)
          }
      )
  }

  /** String based input/output methods
   */
  object StringBased {
    /** Using ARM, pulls the content through an InputStreamReader out
     *  of InputStream.
     *
     *  @param inputStream The resource which is to be wrapped by an
     *                     InputStreamReader and through which the
     *                     content is pulled
     *  @param bufferSize  Optionally override the default buffer size
     *  @return            Either Success(String) which holds the
     *                     retrieved content, or Failure(Exception)
     *                     where the Exception was captured by a
     *                     try/catch block
     */
    def fromInputStream(
        inputStream: InputStream
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[String] =
      operate[InputStreamReader, String](
          () => new InputStreamReader(inputStream)
        , (inputStreamReader) =>
            fromReader(inputStreamReader, bufferSize)
      )

    /** Using ARM, pushes the content through an OutputStreamWriter
     *  into an OutputStream.
     *
     *  @param outputStream The resource which is to be wrapped by an
     *                      OutputStreamWriter and through which the
     *                      content is pushed
     *  @param content      Information to persist
     *  @param bufferSize   Optionally override the default buffer size
     *  @return             Either
     *                      Success(completedNoExceptionSingleton) or
     *                      Failure(Exception) where the Exception was
     *                      captured by a try/catch block
     */
    def toOutputStream(
        outputStream: OutputStream
      , content: String
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[CompletedNoException] =
      operate[OutputStreamWriter, CompletedNoException](
          () => new OutputStreamWriter(outputStream)
        , (outputStreamWriter) =>
            toWriter(outputStreamWriter, content, bufferSize)
      )

    /** Using ARM, pulls the content through a BufferedReader out of a
     *  Reader.
     *
     *  @param reader     The resource which is to be wrapped by a
     *                    BufferedReader and through which the content
     *                    is pulled
     *  @param bufferSize Optionally override the default buffer size
     *  @return           Either Success(String) which holds the
     *                    retrieved content, or Failure(Exception)
     *                    where the Exception was captured by a
     *                    try/catch block
     */
    def fromReader(
        reader: Reader
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[String] =
      ListStringBased.fromReader(reader, bufferSize).flatMap(
        (lines) =>
          Success(
            lines.mkString(
              Option(
                System.getProperty("line.separator")
              ).getOrElse("")
            )
          )
      )

    /** Using ARM, pushes the content as a String through BufferedWriter
     *  into a Writer
     *
     *  @param writer     The resource into which to push the content
     *                    through a BufferedWriter
     *  @param content    Information to persist
     *  @param bufferSize Optionally override the default buffer size
     *  @return           Either Success(completedNoExceptionSingleton)
     *                    or Failure(Exception) where the Exception
     *                    was captured by a try/catch block
     */
    def toWriter(
        writer: Writer
      , content: String
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[CompletedNoException] =
      operate[BufferedWriter, CompletedNoException](
        () => new BufferedWriter(writer, bufferSize)
        , (bufferedWriter) => {
          bufferedWriter.write(content)
          bufferedWriter.flush()
          Success(completedNoExceptionSingleton)
        }

      )
  }

  /** List[String] based input/output methods
   */
  object ListStringBased {
    /** Using ARM, pulls the content through an
     *  InputStreamReader out of InputStream.
     *
     *  @param inputStream The resource which is to be wrapped by an
     *                     InputStreamReader and through which the
     *                     content is pulled
     *  @param bufferSize  Optionally override the default buffer size
     *  @return            Either Success(List[String]) which holds the
     *                     retrieved content, or Failure(Exception)
     *                     where the Exception was captured by a
     *                     try/catch block
     */
    def fromInputStream(
        inputStream: InputStream
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[List[String]] =
      operate[InputStreamReader, List[String]](
          () => new InputStreamReader(inputStream)
        , (inputStreamReader) =>
            fromReader(inputStreamReader, bufferSize)
      )

    /** Using ARM, pushes the content through an OutputStreamWriter
     *  into an OutputStream.
     *
     *  @param outputStream The resource which is to be wrapped by an
     *                      OutputStreamWriter and through which the
     *                      content is pushed
     *  @param content      Information to persist
     *  @param bufferSize   Optionally override the default buffer size
     *  @return             Either
     *                      Success(completedNoExceptionSingleton) or
     *                      Failure(Exception) where the Exception was
     *                      captured by a try/catch block
     */
    def toOutputStream(
        outputStream: OutputStream
      , content: List[String]
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[CompletedNoException] =
      operate[OutputStreamWriter, CompletedNoException](
          () => new OutputStreamWriter(outputStream)
        , (outputStreamWriter) =>
            toWriter(outputStreamWriter, content, bufferSize)
      )

    /** Using ARM, pulls the content through a BufferedReader out of a
     *  Reader.
     *
     *  @param reader     The resource which is to be wrapped by a
     *                    BufferedReader and through which the content
     *                    is pulled
     *  @param bufferSize Optionally override the default buffer size
     *  @return           Either Success(List[String]) which holds the
     *                    retrieved content, or Failure(Exception)
     *                    where the Exception was captured by a
     *                    try/catch block
     */
    def fromReader(
        reader: Reader
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[List[String]] =
      operate[BufferedReader, List[String]](
          () => new BufferedReader(reader, bufferSize)
        , (bufferedReader) => {
            @tailrec
            def recursive(accumulator: List[String]): List[String] = {
              val line = bufferedReader.readLine()
              if (line == null) accumulator
              else recursive(line :: accumulator)
            }
            Success(recursive(Nil).reverse)
          }
      )

    /** Using ARM, pushes the content through BufferedWriter into a
     *  Writer.
     *
     *  @param writer     The resource into which to push the content
     *                    through a BufferedWriter
     *  @param content    Information to persist; after each String is
     *                    emitted an explicit BufferedWriter.newLine()
     *                    is performed.
     *  @param bufferSize Optionally override the default buffer size
     *  @return           Either Success(completedNoExceptionSingleton)
     *                    or Failure(Exception) where the Exception
     *                    was captured by a try/catch block
     */
    def toWriter(
        writer: Writer
      , content: List[String]
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[CompletedNoException] =
      operate[BufferedWriter, CompletedNoException](
          () => new BufferedWriter(writer, bufferSize)
        , (bufferedWriter) => {
            content.foreach(
              (string) => {
                bufferedWriter.write(string)
                bufferedWriter.newLine()
              }
            )
            bufferedWriter.flush()
            Success(completedNoExceptionSingleton)
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
