package org.scalaolio.java

import java.sql.{Connection, DriverManager, ResultSet, Statement}
import java.sql.{Date, Time, Timestamp}
import javax.naming.InitialContext
import javax.sql.DataSource

import org.joda.time.{DateTime, DateTimeZone}

import scala.util.{Failure, Success, Try}

//TODO: must completely rework to put Try in at much lower levels (with better error messages and logging options)
package object Sql {
  trait DatabaseAccess {
    def getConnection: Try[Connection]
//    def suppressSQLExceptionStackTrace: Boolean
  }

  class DatabaseAccessUrl(
      val driverName: String     //ex: "org.h2.Driver"
    , val connectionUrl: String  //ex: "jdbc:h2:tcp://localhost/~/test"
    , val username: String = ""  //ex: "sa"
    , val password: String = ""  //ex: "sa"
//    , val suppressSQLExceptionStackTrace: Boolean = false
  ) extends DatabaseAccess {
    def getConnection: Try[Connection] =
      Try {
        Class.forName(driverName)
        DriverManager.getConnection(connectionUrl, username, password)
      }
  }

  class DatabaseAccessUrlJndi(
      val name: String
//    , val suppressSQLExceptionStackTrace: Boolean = false
  ) extends DatabaseAccess {
    def getConnection: Try[Connection] = {
      Try(new InitialContext().lookup(name).asInstanceOf[DataSource].getConnection)
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
  }

  private def getTs[T](resultSet: ResultSet, convertToT: ResultSetReadOnlyRow => Try[T], abort: () => Boolean): Try[List[T]] = {
    def processRow(accumulator: List[T]): Try[List[T]] = {
      if (!abort.apply) {
        if (resultSet.next())
          convertToT(new ResultSetReadOnlyRow(resultSet)) match {
            case Success(t) => processRow(t :: accumulator)
            case Failure(e) => Failure(e)
          }
        else
          Success(accumulator)
      }
      else
        Failure(new InterruptedException("aborted during harvesting results"))
    }
    if (!abort.apply) {
      processRow(Nil).flatMap(
        ts => Success(ts.reverse)
      )
    }
    else
      Failure(new InterruptedException("aborted prior to harvesting results"))
  }

  def select[T](
      connection: () => Try[Connection]
    , sql: String
    , convertToT: ResultSetReadOnlyRow => Try[T]
    , statement: Connection => Try[Statement] = connection => Try(connection.createStatement)
    , resultSet: (Statement, String) => Try[ResultSet] = (statement, sql) => Try(statement.executeQuery(sql))
    , convertToTs: (ResultSet, ResultSetReadOnlyRow => Try[T], () => Boolean) => Try[List[T]] = (x, y: ResultSetReadOnlyRow => Try[T], z) => getTs[T](x, y, z)
    , abort: () => Boolean = () => false
  ): Try[List[T]] = {
    var autoCloseables: List[AutoCloseable] = Nil
    try {
      connection().flatMap(
        connection => {
          autoCloseables = connection :: autoCloseables
          statement(connection).flatMap(
            statement => {
              autoCloseables = statement :: autoCloseables
              resultSet(statement, sql).flatMap(
                resultSet => {
                  autoCloseables = resultSet :: autoCloseables
                  convertToTs(resultSet, convertToT, abort)
                }
              )
            }
          )
        }
      )
    }
    finally {
      autoCloseables.foreach(a => Try(a.close()))
    }
  }
}
