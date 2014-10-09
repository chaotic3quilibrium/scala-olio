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

import org.scalaolio.java.lang.AutoCloseable_.{TransferProfile, ManagedAutoCloseable, ManagedAutoCloseableFactoryBase}
import org.scalaolio.util.Try_._

/** Encapsulate with idiomatic Scala all the Java null and checked
 *  exception prone methods in Java.
 */
package object Io_ {
  /** Provides a more modern default than the one in java.io (which
   *  vary from 2048 to 8192).
   */
  val DEFAULT_BUFFER_SIZE = 65536

  /** Extending Push/Pull implementations with buffering.
   *
   *  @tparam F Type from which T is generated
   *  @tparam T Type implementing the
   *            java.lang.AutoCloseable interface
   *  @tparam C Type of the content
   */
  trait TransferProfileBuffered[
      F <: java.lang.AutoCloseable
    , T <: java.lang.AutoCloseable
    , C
  ] extends TransferProfile[F, T, C] {
    /** Abstraction of pulling content from T (via forwarding call to
     *  pullBuffered).
     *
     *  @param from From which T is generated
     *  @return     Either Success(C) where C is content to pull or
     *              Failure(Exception) where Exception can arise from
     *              any of the function executions of autoCloseable,
     *              transfer and to.close()
     */
    override def pull(
        from: F
    ): Try[C] =
      pullBuffered(from)

    /** Abstraction of pushing content into T (via forwarding call to
     *  pushBuffered).
     *
     *  @param to      Destination for the content
     *  @param content Content to push
     *  @return        Either Success(completedNoExceptionSingleton) or
     *                 Failure(Exception) where the Exception was
     *                 captured by a try/catch block
     */
    override def push(
        to: T
      , content: C
    ): Try[CompletedNoException] =
      pushBuffered(to, content)
    /** Abstraction of pulling content from T via buffer.
     *
     *  @param from From which T is generated
     *  @return     Either Success(C) where C is content to pull or
     *              Failure(Exception) where Exception can arise from
     *              any of the function executions of autoCloseable,
     *              transfer and to.close()
     */
    def pullBuffered(
        from: F
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[C]

    /** Abstraction of pushing content into T via buffer.
     *
     *  @param to      Destination for the content
     *  @param content Content to push
     *  @return        Either Success(completedNoExceptionSingleton) or
     *                 Failure(Exception) where the Exception was
     *                 captured by a try/catch block
     */
    def pushBuffered(
        to: T
      , content: C
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[CompletedNoException]
  }

  /** Anchor for byte based Push/Pull implementations.
   *
   *  @tparam F Type from which T is generated
   *  @tparam T Type implementing the
   *            java.lang.AutoCloseable interface
   *  @tparam C Type of the content (byte based)
   */
  trait TransferProfileByte[
      F <: InputStream
    , T <: OutputStream
    , C
  ] extends TransferProfile[F, T, C]

  /** Anchor for byte based Push/Pull implementations via buffering.
   *
   *  @tparam F Type from which T is generated
   *  @tparam T Type implementing the
   *            java.lang.AutoCloseable interface
   *  @tparam C Type of the content (byte based)
   */
  trait TransferProfileByteBuffered[
      F <: BufferedInputStream
    , T <: BufferedOutputStream
    , C
  ] extends TransferProfileByte[F, T, C]
    with TransferProfileBuffered[F, T, C]

  /** Anchor for character based Push/Pull implementations.
   *
   *  @tparam F Type from which T is generated
   *  @tparam T Type implementing the
   *            java.lang.AutoCloseable interface
   *  @tparam C Type of the content (character based)
   */
  trait TransferProfileChar[
      F <: Reader
    , T <: Writer
    , C
  ] extends TransferProfile[F, T, C]

  /** Anchor for character based Push/Pull implementations via
   *  buffering.
   *
   *  @tparam F Type from which T is generated
   *  @tparam T Type implementing the
   *            java.lang.AutoCloseable interface
   *  @tparam C Type of the content (character based)
   */
  trait TransferProfileCharBuffered[
      F <: BufferedReader
    , T <: BufferedWriter
    , C
  ] extends TransferProfileChar[F, T, C]
    with TransferProfileBuffered[F, T, C]

  /** Concrete Array[Byte] implementation methods for push/pull.
   */
  trait TransferArrayByte
    extends TransferProfileByte[
        InputStream
      , OutputStream
      , Array[Byte]
    ] {
    /** Pulls the content from a InputStream
     *
     *  @param from The resource from which the content is pulled
     *  @return     Either Success(Array[Byte]) which holds the
     *              retrieved content, or Failure(Exception) where the
     *              Exception was captured by a try/catch block
     */
    def pull(
        from: InputStream
    ): Try[Array[Byte]] =
      pullBuffered(from)

    /** Pushes the content to a OutputStream
     *
     *  @param to      The resource to which the content is pushed
     *  @param content Information to persist
     *  @return        Either
     *                 Success(completedNoExceptionSingleton) or
     *                 Failure(Exception) where the Exception was
     *                 captured by a try/catch block
     */
    def push(
        to: OutputStream
      , content: Array[Byte]
    ): Try[CompletedNoException] = {
      to.write(content)
      Success(completedNoExceptionSingleton)
    }

    /** Pulls the content from a InputStream
     *
     *  @param from       The resource from which the content is
     *                    pulled
     *  @param bufferSize Optionally override the default buffer
     *                    size
     *  @return           Either Success(Array[Byte]) which holds
     *                    the retrieved content, or
     *                    Failure(Exception) where the Exception
     *                    was captured by a try/catch block
     */
    def pullBuffered(
        from: InputStream
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[Array[Byte]] = {
      //read data out of stream
      @tailrec
      def recursive(
          accumulator: List[(Array[Byte], Int)]
      ): List[(Array[Byte], Int)] = {
        val buffer = new Array[Byte](bufferSize)
        //- from.read can fetch less than
        //  bufferSize bytes and still not be at the end of the
        //  file
        val bytesRead = from.read(buffer)
        if (bytesRead < 1) accumulator
        else recursive((buffer, bytesRead) :: accumulator)
      }
      // Concatenate a sequence of byte buffers into a single one.
      def flatten(
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
      Success(flatten(recursive(Nil).reverse))
    }
  }

  /** Concrete String implementation methods for push/pull.
   */
  trait TransferString
    extends TransferProfileChar[Reader, Writer, String] {
    /** Pulls the content from a Reader
     *
     *  @param from The resource from which the content is pulled
     *  @return     Either Success(String) which holds the retrieved
     *              content, or Failure(Exception) where the
     *              Exception was captured by a try/catch block
     */
    def pull(
        from: Reader
    ): Try[String] = {
      pullBuffered(from)
    }

    /** Pushes the content to a Writer
     *
     *  @param to      The resource to which the content is pushed
     *  @param content Information to persist
     *  @return        Either Success(completedNoExceptionSingleton) or
     *                 Failure(Exception) where the Exception was
     *                 captured by a try/catch block
     */
    def push(
        to: Writer
      , content: String
    ): Try[CompletedNoException] = {
      to.write(content)
      to.flush()
      Success(completedNoExceptionSingleton)
    }

    /** Pulls the content from a Reader with a specified read buffer
     *  size
     *
     *  @param from       The resource from which the content is
     *                    pulled
     *  @param bufferSize Optionally override the default buffer size
     *  @return           Either Success(String) which holds the
     *                    retrieved content, or Failure(Exception)
     *                    where the Exception was captured by a
     *                    try/catch block
     */
    def pullBuffered(
        from: Reader
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
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

  /** Concrete List[String] implementation methods for push/pull.
   */
  trait TransferListString
    extends TransferProfileCharBuffered[
        BufferedReader
      , BufferedWriter
      , List[String]
    ] {
    /** Pulls the content from a BufferedReader
     *
     *  @param from       The resource from which the content is
     *                    pulled
     *  @param bufferSize Optionally override the default buffer
     *                    size
     *  @return           Either Success(List[String]) which holds
     *                    the retrieved content, or
     *                    Failure(Exception) where the Exception
     *                    was captured by a try/catch block
     */
    def pullBuffered(
        from: BufferedReader
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[List[String]] =
      Try {
        @tailrec
        def recursive(accumulator: List[String]): List[String] = {
          val line = from.readLine()
          if (line == null) accumulator
          else recursive(line :: accumulator)
        }
        recursive(Nil).reverse
      }

    /** Pushes the content to a BufferedWriter
     *
     *  @param to      The resource to which the content is pushed
     *  @param content Information to persist
     *  @return        Either Success(completedNoExceptionSingleton) or
     *                 Failure(Exception) where the Exception was
     *                 captured by a try/catch block
     */
    def pushBuffered(
        to: BufferedWriter
      , content: List[String]
      , bufferSize: Int = DEFAULT_BUFFER_SIZE
    ): Try[CompletedNoException] = {
      content.foreach(
        (string) => {
          to.write(string)
          to.newLine()
        }
      )
      to.flush()
      Success(completedNoExceptionSingleton)
    }
  }

  /** Concrete Serializable implementation methods for push/pull.
   */
  trait TransferSerializable
    extends TransferProfileByte[
        ObjectInputStream
      , ObjectOutputStream
      , Serializable
    ] {
    /** Pulls the content from a ObjectInputStream
     *
     *  @param from The resource from which the content is pulled
     *  @return     Either Success(Serializable) which holds the
     *              retrieved content, or Failure(Exception) where the
     *              Exception was captured by a try/catch block
     */
    def pull(
        from: ObjectInputStream
    ): Try[Serializable] =
      Try(from.readObject().asInstanceOf[Serializable])

    /** Pushes the content to a ObjectOutputStream
     *
     *  @param to      The resource to which the content is pushed
     *  @param content Information to persist
     *  @return        Either Success(completedNoExceptionSingleton) or
     *                 Failure(Exception) where the Exception was
     *                 captured by a try/catch block
     */
    def push(
        to: ObjectOutputStream
      , content: Serializable
    ): Try[CompletedNoException] = {
      to.writeObject(content)
      to.flush()
      Success(completedNoExceptionSingleton)
    }
  }

  /** Defines a deferred contructor concrete implementation factory for
   *  any InputStream descendant to be wrapped by a
   *  BufferedInputStream.
   *
   *  @tparam F The type to wrap with a buffer
   */
  trait FactoryBufferedInputStream[F <: InputStream]
    extends ManagedAutoCloseableFactoryBase[F, BufferedInputStream] {
    def bufferSize: Int
    def autoCloseable: (F) => Try[BufferedInputStream] =
      (inputStream) =>
        Try(new BufferedInputStream(inputStream, bufferSize))
  }

  /** Defines a deferred contructor concrete implementation factory for
   *  any OutputStream descendant to be wrapped by a
   *  BufferedOutputStream.
   *
   *  @tparam F The type to wrap with a buffer
   */
  trait FactoryBufferedOutputStream[F <: OutputStream]
    extends ManagedAutoCloseableFactoryBase[F, BufferedOutputStream] {
    def bufferSize: Int
    def autoCloseable: (F) => Try[BufferedOutputStream] =
      (outputStream) =>
        Try(new BufferedOutputStream(outputStream, bufferSize))
  }

  /** Defines a deferred contructor concrete implementation factory for
   *  any InputStream descendant to be wrapped by a
   *  ObjectInputStream.
   *
   *  @tparam F The type to wrap with a buffer
   */
  trait FactoryObjectInputStream[F <: InputStream]
    extends ManagedAutoCloseableFactoryBase[F, ObjectInputStream] {
    def autoCloseable: (InputStream) => Try[ObjectInputStream] =
      (inputStream) =>
        Try(new ObjectInputStream(inputStream))
  }

  /** Defines a deferred contructor concrete implementation factory for
   *  any OutputStream descendant to be wrapped by a
   *  ObjectOutputStream.
   *
   *  @tparam F The type to wrap with a buffer
   */
  trait FactoryObjectOutputStream[F <: OutputStream]
    extends ManagedAutoCloseableFactoryBase[F, ObjectOutputStream] {
    def autoCloseable: (OutputStream) => Try[ObjectOutputStream] =
      (outputStream) =>
        Try(new ObjectOutputStream(outputStream))
  }

  /** Defines a deferred contructor concrete implementation factory for
   *  any Reader descendant to be wrapped by a BufferedReader.
   *
   *  @tparam F The type to wrap with a buffer
   */
  trait FactoryBufferedReader[F <: Reader]
    extends ManagedAutoCloseableFactoryBase[F, BufferedReader] {
    def bufferSize: Int
    def autoCloseable: (F) => Try[BufferedReader] =
      (reader) =>
        Try(new BufferedReader(reader, bufferSize))
  }

  /** Defines a deferred contructor concrete implementation factory for
   *  any Writer descendant to be wrapped by a BufferedWriter.
   *
   *  @tparam F The type to wrap with a buffer
   */
  trait FactoryBufferedWriter[F <: Writer]
    extends ManagedAutoCloseableFactoryBase[F, BufferedWriter] {
    def bufferSize: Int
    def autoCloseable: (F) => Try[BufferedWriter] =
      (writer) =>
        Try(new BufferedWriter(writer, bufferSize))
  }

  /**
   *
   *  @tparam F
   *  @tparam I
   */
  trait BufferedInputStreamBase[F <: InputStream, I]
    extends ManagedAutoCloseable[F, BufferedInputStream, Nothing, I]
    with FactoryBufferedInputStream[F]

  /**
   *
   *  @tparam F
   *  @tparam O
   */
  trait BufferedOutputStreamBase[F <: OutputStream, O]
    extends ManagedAutoCloseable[
      F, BufferedOutputStream, O, CompletedNoException
    ]
    with FactoryBufferedOutputStream[F]

  /**
   *
   *  @tparam F
   */
  trait ObjectInputStreamBase[F <: InputStream]
    extends ManagedAutoCloseable[F, ObjectInputStream, Nothing, Serializable]
    with FactoryObjectInputStream[F]

  /**
   *
   *  @tparam F
   */
  trait ObjectOutputStreamBase[F <: OutputStream]
    extends ManagedAutoCloseable[
      F, ObjectOutputStream, Serializable, CompletedNoException
    ]
    with FactoryObjectOutputStream[F]

  /**
   *
   *  @tparam F
   *  @tparam I
   */
  trait BufferedReaderBase[F <: Reader, I]
    extends ManagedAutoCloseable[F, BufferedReader, Nothing, I]
    with FactoryBufferedReader[F]

  /**
   *
   *  @tparam F
   *  @tparam O
   */
  trait BufferedWriterBase[F <: Writer, O]
    extends ManagedAutoCloseable[
      F, BufferedWriter, O, CompletedNoException
    ]
    with FactoryBufferedWriter[F]

  /**
   *
   *  @param bufferSize
   *  @tparam F
   */
  class BufferedInputStreamArrayByte[F <: InputStream](
        val bufferSize: Int = DEFAULT_BUFFER_SIZE
    ) extends BufferedInputStreamBase[F, Array[Byte]]
      with TransferArrayByte {
    override def transfer:
      (BufferedInputStream, Option[Nothing]) => Try[Array[Byte]] =
      (bufferedInputStream, nothing) =>
        pullBuffered(bufferedInputStream, bufferSize)
  }

  /**
   *
   *  @param bufferSize
   *  @tparam F
   */
  class BufferedOutputStreamArrayByte[F <: OutputStream](
        val bufferSize: Int = DEFAULT_BUFFER_SIZE
    ) extends BufferedOutputStreamBase[F, Array[Byte]]
      with TransferArrayByte {
    override def transfer:
      (BufferedOutputStream, Option[Array[Byte]]) => Try[CompletedNoException] =
      (bufferedOutputStream, content) =>
        push(bufferedOutputStream, content.get)
  }

  /**
   *
   *  @tparam F
   */
  class InputStreamSerializable[F <: InputStream]
    extends ObjectInputStreamBase[F]
    with TransferSerializable {
    override def transfer:
      (ObjectInputStream, Option[Nothing]) => Try[Serializable] =
      (objectInputStream, nothing) =>
        pull(objectInputStream)
  }

  /**
   *
   *  @tparam F
   */
  class OutputStreamSerializable[F <: OutputStream]
    extends ObjectOutputStreamBase[F]
    with TransferSerializable {
    override def transfer:
      (ObjectOutputStream, Option[Serializable]) => Try[CompletedNoException] =
      (objectOutputStream, content) =>
        push(objectOutputStream, content.get)
  }

  /**
   *
   *  @param bufferSize
   */
  class BufferedInputStreamSerializable(
        val bufferSize: Int = DEFAULT_BUFFER_SIZE
    ) extends InputStreamSerializable[BufferedInputStream]

  /**
   *
   *  @param bufferSize
   */
  class BufferedOutputStreamSerializable(
        val bufferSize: Int = DEFAULT_BUFFER_SIZE
    ) extends OutputStreamSerializable[BufferedOutputStream]

  /**
   *
   *  @param bufferSize
   *  @tparam F
   */
  class BufferedReaderString[F <: Reader](
        val bufferSize: Int = DEFAULT_BUFFER_SIZE
    ) extends BufferedReaderBase[F, String]
      with TransferString {
    override def transfer:
      (BufferedReader, Option[Nothing]) => Try[String] =
      (bufferedReader, nothing) =>
        pullBuffered(bufferedReader, bufferSize)
  }

  /**
   *
   *  @param bufferSize
   *  @tparam F
   */
  class BufferedWriterString[F <: Writer](
        val bufferSize: Int = DEFAULT_BUFFER_SIZE
    ) extends BufferedWriterBase[F, String]
      with TransferString {
    override def transfer:
      (BufferedWriter, Option[String]) => Try[CompletedNoException] =
      (bufferedWriter, content) =>
        push(bufferedWriter, content.get)
  }

  /**
   *
   *  @param bufferSize
   *  @tparam F
   */
  class BufferedReaderListString[F <: Reader](
      val bufferSize: Int = DEFAULT_BUFFER_SIZE
  ) extends BufferedReaderBase[F, List[String]]
    with TransferListString {
    override def transfer:
      (BufferedReader, Option[Nothing]) => Try[List[String]] =
      (bufferedReader, nothing) =>
        pullBuffered(bufferedReader, bufferSize)
  }

  /**
   *
   *  @param bufferSize
   *  @tparam F
   */
  class BufferedWriterListString[F <: Writer](
      val bufferSize: Int = DEFAULT_BUFFER_SIZE
  ) extends BufferedWriterBase[F, List[String]]
    with TransferListString {
    override def transfer:
      (BufferedWriter, Option[List[String]]) => Try[CompletedNoException] =
      (bufferedWriter, content) =>
        push(bufferedWriter, content.get)
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
