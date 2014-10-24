/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.java.Sql                                  **
**   Name:      package.scala                                           **
**                                                                      **
** Description:                                                         **
**  Enables access to JDBC data via idiomatic Scala                     **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2014 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.java

import java.sql.{Connection, DriverManager, ResultSet, Statement}
import java.sql.{Date, Time, Timestamp}
import javax.naming.InitialContext
import javax.sql.DataSource

import grizzled.slf4j.Logger
import org.joda.time.{DateTime, DateTimeZone}
import org.scalaolio.util.Try_.CompletedNoException

import scala.util.{Failure, Success, Try}

//TODO: remove use of var for tracking java.lang.AutoCloseable instances - move ASAP to using AutoCloseable_
package object Sql {
  private val LOGGER = Logger(getClass.getName)

  trait DatabaseAccess {
    def getConnection: Try[Connection]
    def suppressSQLExceptionLogging: Boolean
  }

  def errorOut[T](message: String, suppressSQLExceptionLogging: Boolean, throwable: Option[Throwable] = None): Failure[T] = {
    if (!suppressSQLExceptionLogging) LOGGER.error(message)
    Failure(
      throwable match {
        case Some(throwableGet) =>
          new IllegalStateException(message, throwableGet)
        case None =>
          new IllegalStateException(message)
      }
    )
  }

  class DatabaseAccessUrl(
      val driverName: String     //ex: "org.h2.Driver"
    , val connectionUrl: String  //ex: "jdbc:h2:tcp://localhost/~/test"
    , val username: String = ""  //ex: "sa"
    , val password: String = ""  //ex: "sa"
    , val suppressSQLExceptionLogging: Boolean = false
  ) extends DatabaseAccess {
    def getConnection: Try[Connection] =
      Try(Class.forName(driverName)) match {
        case Success(_) =>
          Try(DriverManager.getConnection(connectionUrl, username, password)) match {
            case Success(connection) =>
              Success(connection)
            case Failure(throwable) =>
              errorOut[Connection](
                  s"DriverManager.getConnection($connectionUrl, $username, PASSWORD_SUPPRESSED) failed - original exception: ${throwable.getMessage}"
                , suppressSQLExceptionLogging
                , Some(throwable)
              )
          }
        case Failure(throwable) =>
          errorOut[Connection](
              s"Class.forName($driverName) failed - original exception: ${throwable.getMessage}"
            , suppressSQLExceptionLogging
            , Some(throwable)
          )
      }
  }

  class DatabaseAccessUrlJndi(
      val name: String
    , val suppressSQLExceptionLogging: Boolean = false
  ) extends DatabaseAccess {
    def getConnection: Try[Connection] = {
      Try(new InitialContext()) match {
        case Success(initialContext) =>
          Try(initialContext.lookup(name)) match {
            case Success(anyRef) =>
              Try(anyRef.asInstanceOf[DataSource]) match {
                case Success(dataSource) =>
                  Try(dataSource.getConnection)
                case Failure(throwable) =>
                  errorOut[Connection](
                      s"dataSource.getConnection failed for jndiName [$name] - original exception: ${throwable.getMessage}"
                    , suppressSQLExceptionLogging
                    , Some(throwable)
                  )
              }
            case Failure(throwable) =>
              errorOut[Connection](
                  s"initialContext.lookup() failed for jndiName [$name] - original exception: ${throwable.getMessage}"
                , suppressSQLExceptionLogging
                , Some(throwable)
              )
          }
        case Failure(throwable) =>
          errorOut[Connection](
              s"initialContext failed for jndiName [$name] - original exception: ${throwable.getMessage}"
            , suppressSQLExceptionLogging
            , Some(throwable)
          )
      }
    }
  }

  //immutable interface to the current row in resultSet
  class ResultSetReadOnlyRow(private val resultSet: ResultSet) {
    //primitives
    def getBoolean(index: Int): Boolean = resultSet.getBoolean(index)
    def getBoolean(columnLabel: String): Boolean = resultSet.getBoolean(columnLabel)
    def getByte(index: Int): Byte = resultSet.getByte(index)
    def getByte(columnLabel: String): Byte = resultSet.getByte(columnLabel)
    def getShort(index: Int): Short = resultSet.getShort(index)
    def getShort(columnLabel: String): Short = resultSet.getShort(columnLabel)
    def getChar(index: Int): Char = resultSet.getString(index).charAt(0)
    def getChar(columnLabel: String): Char = resultSet.getString(columnLabel).charAt(0)
    def getInt(index: Int): Int = resultSet.getInt(index)
    def getInt(columnLabel: String): Int = resultSet.getInt(columnLabel)
    def getLong(index: Int): Long = resultSet.getLong(index)
    def getLong(columnLabel: String): Long = resultSet.getLong(columnLabel)
    def getFloat(index: Int): Float = resultSet.getFloat(index)
    def getFloat(columnLabel: String): Float = resultSet.getFloat(columnLabel)
    def getDouble(index: Int): Double = resultSet.getDouble(index)
    def getDouble(columnLabel: String): Double = resultSet.getDouble(columnLabel)

    //references
    def getString(index: Int): String = resultSet.getString(index)
    def getString(columnLabel: String): String = resultSet.getString(columnLabel)
    def getDateTimeUtc(index: Int): DateTime = new DateTime(resultSet.getDate(index), DateTimeZone.UTC)
    def getDateTimeUtc(columnLabel: String): DateTime = new DateTime(resultSet.getDate(columnLabel), DateTimeZone.UTC)
    def getDateTimeLocal(index: Int): DateTime = new DateTime(resultSet.getDate(index))
    def getDateTimeLocal(columnLabel: String): DateTime = new DateTime(resultSet.getDate(columnLabel))
    def getDate(index: Int): Date = resultSet.getDate(index)
    def getDate(columnLabel: String): Date = resultSet.getDate(columnLabel)
    def getTime(index: Int): Time = resultSet.getTime(index)
    def getTime(columnLabel: String): Time = resultSet.getTime(columnLabel)
    def getTimestamp(index: Int): Timestamp = resultSet.getTimestamp(index)
    def getTimestamp(columnLabel: String): Timestamp = resultSet.getTimestamp(columnLabel)

    case class RowContent(index: Int, name: String, value: String) {
      def toStringRaw: String = s"i=$index;n=$name;v=$value"
    }

    lazy val rowContents: List[RowContent] = {
      val metaData = resultSet.getMetaData
      ( for {
          index <- 1 to metaData.getColumnCount
          name = metaData.getColumnName(index)
          value = Option(getString(index)).getOrElse("<null>")
        } yield RowContent(index, name, value)
      ).toList
    }

    lazy val rowContentByIndex: Map[Int, RowContent] =
      rowContents.map(x => (x.index, x)).toMap

    lazy val rowContentByName: Map[String, RowContent] =
      rowContents.map(x => (x.name, x)).toMap
  }

  private def convertToTsDefault[T](resultSet: ResultSet, convertToT: ResultSetReadOnlyRow => Try[Option[T]], abort: () => Boolean, suppressSQLExceptionLogging: Boolean): Try[List[T]] = {
    def processRow(accumulator: List[T]): Try[List[T]] = {
      if (!abort.apply) {
        if (resultSet.next()) {
          val resultSetReadOnlyRow = new ResultSetReadOnlyRow(resultSet)
          convertToT(resultSetReadOnlyRow) match {
            case Success(optionT) =>
              optionT match {
                case Some(t) => processRow(t :: accumulator)
                case None => processRow(accumulator)
              }
            case Failure(throwable) =>
              val rowContents: String =
                resultSetReadOnlyRow.rowContents.map(_.toStringRaw).mkString("|")
              errorOut[List[T]](
                  s"convertToT failed on the row's contents [$rowContents] - original exception: ${throwable.getMessage}"
                , suppressSQLExceptionLogging
                , Some(throwable)
              )
          }
        }
        else
          Success(accumulator)
      }
      else
        errorOut[List[T]](
            "aborted during harvesting results"
          , suppressSQLExceptionLogging
        )
    }
    if (!abort.apply) {
      processRow(Nil).flatMap(
        ts => Success(ts.reverse)
      )
    }
    else
      errorOut[List[T]](
          "aborted prior to harvesting results"
        , suppressSQLExceptionLogging
      )
  }

  trait Process[R, V] {
    def databaseAccess: DatabaseAccess
    def statement: Connection => Try[Statement]
    def result: (Statement, String) => Try[R]
    def abort: () => Boolean

    protected def specialization(statement: Statement): Try[V]
    protected var autoCloseables: List[AutoCloseable] = Nil //TODO: Replace with java.lang.AutoCloseable_'s ARM system ASAP

    private def opens(): Try[Statement] =
      databaseAccess.getConnection.flatMap(
        connection => {
          autoCloseables = connection :: autoCloseables
          statement(connection).flatMap(
            statement => {
              autoCloseables = statement :: autoCloseables
              Success(statement)
            }
          )
        }
      )

    private def closes(): Unit = {
      autoCloseables.foreach(
        autoCloseable => {
          Try(autoCloseable.close()) match {
            case Success(_) =>
            case Failure(throwable) =>
              errorOut[CompletedNoException](
                  s"close failed - original exception: ${throwable.getMessage}"
                , databaseAccess.suppressSQLExceptionLogging
                , Some(throwable)
              )
          }
        }
      )
    }

    val content: Try[V] = {
      try{
        opens().flatMap(
          statement =>
            specialization(statement)
        )
      }
      finally {
        closes()
      }
    }
  }

  final class Select[T](
      val databaseAccess: DatabaseAccess
    , val sql: String
    , val convertToT: ResultSetReadOnlyRow => Try[Option[T]]
    , val statement: Connection => Try[Statement] = connection => Try(connection.createStatement)
    , val result: (Statement, String) => Try[ResultSet] = (statement, sql) => Try(statement.executeQuery(sql))
    , val abort: () => Boolean = () => false
  ) extends Process[ResultSet, List[T]] {
    override protected def specialization(statement: Statement): Try[List[T]] = {
      result(statement, sql).flatMap(
        resultSet => {
          autoCloseables = resultSet :: autoCloseables
          convertToTsDefault(resultSet, convertToT, abort, databaseAccess.suppressSQLExceptionLogging)
        }
      )
    }
  }

  final class Update(
      val databaseAccess: DatabaseAccess
    , val sql: String
    , val statement: Connection => Try[Statement] = connection => Try(connection.createStatement)
    , val result: (Statement, String) => Try[Int] = (statement, sql) => Try(statement.executeUpdate(sql))
    , val abort: () => Boolean = () => false
  ) extends Process[Int, Int] {
    override protected def specialization(statement: Statement): Try[Int] =
      result(statement, sql)
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
