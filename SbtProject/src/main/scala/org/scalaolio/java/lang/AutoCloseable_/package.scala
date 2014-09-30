/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.java.lang.AutoCloseable_                  **
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
package org.scalaolio.java.lang

import scala.util.{Failure, Try}

/** Encapsulate with idiomatic Scala all the Java null and checked
  *  exception prone methods in Java.
  */
package object AutoCloseable_ {
  /** Implements core ARM (Automatic Resource Management) pattern which
   *  is provided starting in Java 7 (1.7).
   *
   *  @param autoCloseable  Function returning an instance of resource
   *                        A needing to be ARMed
   *  @param putContent     Function taking A as input and returning a
   *                        Try wrapped B as output
   *  @tparam A             An instance of a resource implementing
   *                        java.lang.AutoCloseable
   *  @tparam B             The output target of the function; for
   *                        methods emitting output (ex: to the file
   *                        system), completedNoExceptionSingleton can
   *                        be returned
   *  @return               Either Success(B) or Failure(Exception)
   *                        where the Exception can arise from any of
   *                        the function executions of autoCloseable,
   *                        putContent and AutoCloseable.close()
   */
  def operate[A <: java.lang.AutoCloseable, B](
      autoCloseable: () => A
    , putContent: (A) => Try[B]
  ): Try[B] =
    Try(autoCloseable.apply()).flatMap(
      a => {
        var exceptionPutContent: Option[Exception] = None
        var exceptionClose: Option[Exception] = None
        var tryB: Try[B] =
          Failure(
            new IllegalStateException(
                "should never get here - tryB is None - another"
              + " Exception should have prevented this state"
            )
          )
        try {
          try {
            tryB = putContent(a)
          } catch {
            case exception: Exception =>
              exceptionPutContent = Some(exception)
          }
        } finally {
          try {
            a.close()
          } catch {
            case exception: Exception =>
              exceptionClose = Some(exception)
          }
        }
        exceptionPutContent match {
          case Some(exception) => Failure(exception)
          case None =>
            exceptionClose match {
              case Some(exception) => Failure(exception)
              case None => tryB
            }
        }
      }
    )
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
