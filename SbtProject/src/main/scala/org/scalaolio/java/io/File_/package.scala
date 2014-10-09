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

import org.scalaolio.util.Try_.CompletedNoException

import scala.util.{Failure, Try}

import org.scalaolio.java.Io_._
import org.scalaolio.java.lang.AutoCloseable_.{ManagedAutoCloseableNested, ManagedAutoCloseable, ManagedAutoCloseableFactoryBase}

/** Encapsulate with idiomatic Scala all the Java null and checked
 *  exception prone methods in Java.
 */
package object File_ {

  /**
   *
   */
  trait FactoryFileReaderFromFile
    extends ManagedAutoCloseableFactoryBase[File, FileReader] {
    /**
     *
     *  @return
     */
    def autoCloseable: (File) => Try[FileReader] =
      (file) =>
        Try(new FileReader(file))
  }

  /**
   *
   */
  trait FactoryFileReaderFromFileDescriptor
    extends ManagedAutoCloseableFactoryBase[FileDescriptor, FileReader] {
    /**
     *
     *  @return
     */
    def autoCloseable: (FileDescriptor) => Try[FileReader] =
      (fileDescriptor) =>
        Try(new FileReader(fileDescriptor))
  }

  /**
   *
   */
  trait FactoryFileWriterFromFile
    extends ManagedAutoCloseableFactoryBase[File, FileWriter] {
    /**
     *
     *  @return
     */
    def append: Boolean

    /**
     *
     * @return
     */
    def autoCloseable: (File) => Try[FileWriter] =
      (file) =>
        Try(new FileWriter(file))
  }

  /**
   *
   */
  trait FactoryFileWriterFromFileDescriptor
    extends ManagedAutoCloseableFactoryBase[FileDescriptor, FileWriter] {
    /**
     *
     *  @return
     */
    def autoCloseable: (FileDescriptor) => Try[FileWriter] =
      (fileDescriptor) =>
        Try(new FileWriter(fileDescriptor))
  }

  /**
   *
   *  @param managedAutoCloseable
   *  @tparam T  Type implementing the
   *             java.lang.AutoCloseable interface
   *  @tparam I  Content to pull in
   */
  class FileReaderFromFileNested[T <: Reader, I] (
      val managedAutoCloseable:
        ManagedAutoCloseable[FileReader, T, Nothing, I]
  ) extends ManagedAutoCloseableNested[File, FileReader, T, Nothing, I]
    with FactoryFileReaderFromFile

  /**
   *
   *  @param managedAutoCloseable
   *  @tparam T  Type implementing the
   *             java.lang.AutoCloseable interface
   *  @tparam I  Content to pull in
   */
  class FileReaderFromFileDescriptorNested[T <: Reader, I] (
      val managedAutoCloseable:
        ManagedAutoCloseable[FileReader, T, Nothing, I]
  ) extends ManagedAutoCloseableNested[FileDescriptor, FileReader, T, Nothing, I]
    with FactoryFileReaderFromFileDescriptor

  /**
   *
   *  @param managedAutoCloseable
   *  @param append
   *  @tparam T  Type implementing the
   *             java.lang.AutoCloseable interface
   *  @tparam O  Content to push out
   */
  class FileWriterFromFileNested[T <: Writer, O] (
      val managedAutoCloseable:
        ManagedAutoCloseable[FileWriter, T, O, CompletedNoException]
    , val append: Boolean = false
  ) extends ManagedAutoCloseableNested[File, FileWriter, T, O, CompletedNoException]
    with FactoryFileWriterFromFile

  /**
   *
   *  @param managedAutoCloseable
   *  @tparam T  Type implementing the
   *             java.lang.AutoCloseable interface
   *  @tparam O  Content to push out
   */
  class FileWriterFromFileDescriptorNested[T <: Writer, O] (
      val managedAutoCloseable:
        ManagedAutoCloseable[FileWriter, T, O, CompletedNoException]
  ) extends ManagedAutoCloseableNested[FileDescriptor, FileWriter, T, O, CompletedNoException]
    with FactoryFileWriterFromFileDescriptor

  /**
   *
   *  @param source
   *  @param bufferSize
   *  @return
   */
  def pullString(
      source: Any
    , bufferSize: Int = DEFAULT_BUFFER_SIZE
  ): Try[String] = {
    val bufferedReaderString =
      new BufferedReaderString[FileReader](bufferSize)
    source match {
      case file: File =>
        (new FileReaderFromFileNested(bufferedReaderString))(file)
      case string: String =>
        Try(new File(string)).flatMap(
          (file) =>
            (new FileReaderFromFileNested(bufferedReaderString))(file)
        )
      case fileDescriptor: FileDescriptor =>
        (new FileReaderFromFileDescriptorNested(bufferedReaderString))(fileDescriptor)
      case None =>
        Failure(new IllegalArgumentException(s"source [$source] case is not defined"))
    }
  }

  /**
   *
   *  @param source
   *  @param content
   *  @param append
   *  @param bufferSize
   *  @return
   */
  def pushString(
      source: Any
    , content: String
    , append: Boolean = false
    , bufferSize: Int = DEFAULT_BUFFER_SIZE
  ): Try[CompletedNoException] = {
    val bufferedWriterString =
      new BufferedWriterString[FileWriter](bufferSize)
    source match {
      case file: File =>
        (new FileWriterFromFileNested(bufferedWriterString, append))(file, Option(content))
      case string: String =>
        Try(new File(string)).flatMap(
          (file) =>
            (new FileWriterFromFileNested(bufferedWriterString, append))(file, Option(content))
        )
      case fileDescriptor: FileDescriptor =>
        (new FileWriterFromFileDescriptorNested(bufferedWriterString))(fileDescriptor)
      case None =>
        Failure(new IllegalArgumentException(s"source [$source] case is not defined"))
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
