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

import org.scalaolio.util.Try_.CompletedNoException

/** Implements the ARM (Automatic Resource Management) pattern
 *  implemented in Java 7 (1.7). Substitutes exception and null prone
 *  Java classes and methods with Scala artifacts (like Try and Option)
 *  which play much more consistently with other Scala idioms
 */
package object AutoCloseable_ {

  /** Defines a deferred contructor abstraction for the ARM (Automatic
   *  Resource Management) pattern which is provided starting in Java 7
   *  (1.7).
   *
   *  @tparam F Type from which T is generated
   *  @tparam T Type implementing the
   *            java.lang.AutoCloseable interface
   */
  trait ManagedAutoCloseableFactoryBase[F, T <: java.lang.AutoCloseable] {
    def autoCloseable: (F) => Try[T]
  }

  /** Defines a deferred execution abstraction for the ARM (Automatic
   *  Resource Management) pattern which is provided starting in Java 7
   *  (1.7).
   *
   *  @tparam F Type from which T is generated
   *  @tparam T Type implementing the
   *            java.lang.AutoCloseable interface
   *  @tparam O Content to push out
   *  @tparam I Content to pull in
   */
  trait ManagedAutoCloseable[
      F
    , T <: java.lang.AutoCloseable
    , O
    , I
  ] extends ManagedAutoCloseableFactoryBase[F, T] {
    /** Function taking a T and Option wrapped O as input and returning
     *  a Try wrapped I as output
     *
     *  @return Function to provide the transform from the source[T]
     *          (and optional content[I]) to the destination[I].
     */
    def transfer: (T, Option[O]) => Try[I]

    /** Executes the pull half of the ARM pattern.
     *
     *  @param from From which T is generated
     *  @return     Either Success(I) where I is content to pull or
     *              Failure(Exception) where Exception can arise from
     *              any of the function executions of autoCloseable,
     *              transfer and to.close()
     */
    def apply(from: F): Try[I] =
      apply(from, None)

    /** Executes both the push and pull of the ARM pattern.
     *
     *  @param from    From which T is generated
     *  @param content Content to push
     *  @return        Either Success(I) where I is content to pull or
     *                 Failure(Exception) where Exception can arise
     *                 from any of the function executions of
     *                 autoCloseable, transfer and to.close()
     */
    def apply(from: F, content: Option[O]): Try[I] =
      autoCloseable(from).flatMap(
        to => {
          var exceptionPutContent: Option[Exception] = None
          var exceptionClose: Option[Exception] = None
          var tryR: Try[I] =
            Failure(
              new IllegalStateException(
                  "should never get here - tryR is None - another"
                + " Exception should have prevented this state"
              )
            )
          try {
            try {
              tryR = transfer(to, content)
            } catch {
              case exception: Exception =>
                exceptionPutContent = Some(exception)
            }
          } finally {
            try {
              to.close()
            } catch {
              case exception: Exception =>
                exceptionClose = Some(exception)
            }
          }
          exceptionPutContent match {
            case Some(exception) =>
              Failure(exception)
            case None =>
              exceptionClose match {
                case Some(exception) => Failure(exception)
                case None => tryR
              }
          }
        }
      )
  }

  /** Defines a nested deferred execution abstraction for the ARM
   *  (Automatic Resource Management) pattern which is provided
   *  starting in Java 7 (1.7).
   *
   *  @tparam F  Type from which T is generated
   *  @tparam T  Type implementing the
   *             java.lang.AutoCloseable interface
   *  @tparam T2 Type implementing the
   *             java.lang.AutoCloseable interface
   *  @tparam O  Content to push out
   *  @tparam I  Content to pull in
   */
  trait ManagedAutoCloseableNested[
      F
    , T <: java.lang.AutoCloseable
    , T2 <: java.lang.AutoCloseable
    , O
    , I
  ] extends ManagedAutoCloseable[F, T, O, I] {
    /** Method descendants must override to provide a nested instance
     *  of ManagedManagedAutoCloseable[Nested] which will be invoked
     *  during this instance's transfer operation.
     *
     *  @return Instance of ManagedAutoCloseable typed strongly to its
     *          owning parent
     */
    def managedAutoCloseable: ManagedAutoCloseable[T, T2, O, I]

    /** Operation to use T to perform push and/or pull of content after
     *  resource F has been used to "open and track" resource T.
     *
     * @return Function to provide the transform from the source[T]
     *         (and optional content[I]) to the destination[I].
     */
    def transfer: (T, Option[O]) => Try[I] =
      (t, o) =>
        managedAutoCloseable(t, o)
  }

  /** Basis for Push/Pull concrete implementations
   *
   *  @tparam F Type from which T is generated
   *  @tparam T Type implementing the
   *            java.lang.AutoCloseable interface
   *  @tparam C Type of the content
   */
  trait TransferProfile[
      F <: java.lang.AutoCloseable
    , T <: java.lang.AutoCloseable
    , C
  ] {
    /** Abstraction of pulling content from T
     *
     *  @param from From which T is generated
     *  @return     Either Success(C) where C is content to pull or
     *              Failure(Exception) where Exception can arise from
     *              any of the function executions of autoCloseable,
     *              transfer and to.close()
     */
    def pull(
        from: F
    ): Try[C]

    /** Abstraction of pushing content into T
     *
     *  @param to      Destination for the content
     *  @param content Content to push
     *  @return        Either Success(completedNoExceptionSingleton) or
     *                 Failure(Exception) where the Exception was
     *                 captured by a try/catch block
     */
    def push(
        to: T
      , content: C
    ): Try[CompletedNoException]
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
