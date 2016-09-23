/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util.trys.template                        **
**   Name:      TryBase.scala                                           **
**                                                                      **
** Description:                                                         **
**  Base trait capturing the common characteristics from which both     **
**  SuccessBase and FailureBase inherit overriding and specializing per **
**  their specific ADT requirements                                     **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util.trys.template

import org.scalaolio.util.FailedPreconditionsException.{FailedPrecondition, FailedPreconditionObject}

object TryBase {
  object FailedPreconditionTryBase extends FailedPreconditionObject[FailedPreconditionTryBase] {
    def apply(
        optionMessage: Option[String] = None
      , optionCause: Option[Throwable] = None
      , isEnableSuppression: Boolean = false
      , isWritableStackTrace: Boolean = false
    ): FailedPreconditionTryBase =
      new FailedPreconditionTryBase(
          optionMessage
        , optionCause
        , isEnableSuppression
        , isWritableStackTrace
      )
  }
  final class FailedPreconditionTryBase private[FailedPreconditionTryBase] (
      optionMessage: Option[String]
    , optionCause: Option[Throwable]
    , isEnableSuppression: Boolean
    , isWritableStackTrace: Boolean
  ) extends
    FailedPrecondition(
        optionMessage
      , optionCause
      , isEnableSuppression
      , isWritableStackTrace
    )
}

/** The `TryBase` type represents a computation that may either result in returning a successfully computed value or an instance of `java.lang.Throwable` (the root class of all `throw`able exceptions within the JVM). It's similar to, but semantically different from the [[scala.util.Either]] type.
  *
  * Instances of `TryBase[T, V]`, are either an instance of a concrete descendant of [[org.scalaolio.util.trys.template.SuccessBase]][T, V] or [[org.scalaolio.util.trys.template.FailureBase]][T, V].
  *
  * For example, `TryObjectBase` can be used to perform division on a user-defined input, without the need to do explicit
  * exception-handling in all of the places that a `java.lang.Throwable` might be thrown.
  *
  * TODO:
  * TODO:
  * TODO:
  * TODO:
  * TODO:
  * TODO:
  *
  * @author Jim O'Flaherty
  * @since 2.11
  */
trait TryBase [T <: Throwable, +V] {
  protected def tryObjectBase: TryObjectBase[T]

  /** Returns `true` if the `TryBase` is a `SuccessBase`, `false` otherwise.
    */
  def isSuccess: Boolean
  /** Returns the value from this `SuccessBase` or throws `FailedPreconditionTryBase` for `FailureBase`.
    */
  def v: V =
    throw TryBase.FailedPreconditionTryBase(s"v is undefined")

  /** Returns `true` if the `TryBase` is a `FailureBase`, `false` otherwise.
    */
  def isFailure: Boolean
  /** Returns `java.lang.Throwable` instance from this `FailureBase` or throws `FailedPreconditionTryBase` for `SuccessBase`.
    */
  def t: T =
    throw TryBase.FailedPreconditionTryBase(s"t is undefined")

  /** Returns the value from this `SuccessBase` or throws the exception if this is a `FailureBase`.
    */
  def get: V
  /** Returns the value from this `SuccessBase` or the given `default` argument if this is a `FailureBase`.
    *
    * ''Note:'': This will throw a `java.lang.Throwable` if it is not a `SuccessBase` and the evaluation of `default` throws a `java.lang.Throwable` instance.
    */
  def getOrElse[W >: V](default: => W): W
  /** Returns a `Some` containing the value if this is a `SuccessBase` or `None` if this is a `FailureBase`.
    */
  def toOption: Option[V]

  /** Returns a `FailureBase`, if it's a `SuccessBase`, containing an `IllegalStateException` if the predicate (function p) is not satisfied.
    *
    * ''Note:'': This will throw a `java.lang.Throwable` if it is a `SuccessBase` and the evaluation of `p` throws a `java.lang.Throwable` instance.
    */
  def filter(p: V => Boolean): TryBase[T, V]
  /** Returns the given function applied to the value from this `SuccessBase` or returns this if this is a `FailureBase`.
    *
    * ''Note:'': This will throw a `java.lang.Throwable` if it is a `SuccessBase` and the evaluation of `f` throws a `java.lang.Throwable` instance.
    */
  def flatMap[W](f: V => TryBase[T, W]): TryBase[T, W]
  /** Transforms a nested `TryBase`, ie, a `TryBase` of type `TryBase[T, TryBase[T, V]]`, into an un-nested `TryBase`, ie, a `TryBase` of type `TryBase[T, V]`.
    */
  def flatten[W](implicit ev: V <:< TryBase[T, W]): TryBase[T, W]
  /** Maps the given function to the value from this `SuccessBase` or returns this if this is a `FailureBase`.
    *
    * ''Note:'': This will throw a `java.lang.Throwable` if it is a `SuccessBase` and the evaluation of `f` throws a `java.lang.Throwable` instance.
    */
  def map[W](f: V => W): TryBase[T, W]
  /** Completes this `TryBase` by applying the function `fv` to this if this is of type `SuccessBase`, or conversely, by applying `ft` if this is a `FailureBase`.
    *
    * ''Note:'': This will throw a `java.lang.Throwable` if it is a `SuccessBase` and the evaluation of `fv` throws a `java.lang.Throwable` instance or as a `FailureBase` and the evaluation of `ft` throws a `java.lang.Throwable` instance.
    */
  def transform[W](fv: V => TryBase[T, W], ft: T => TryBase[T, W]): TryBase[T, W]

  /** Returns this `TryBase` if it's a `SuccessBase` or the given `default` argument if this is a `FailureBase`.
    *
    * ''Note:'': This will throw a `java.lang.Throwable` if it is not a `SuccessBase` and the evaluation of `default` throws a `java.lang.Throwable` instance.
    */
  def orElse[W >: V](default: => TryBase[T, W]): TryBase[T, W]
  /** Applies the given function `f` if this is a `FailureBase`, otherwise returns this if this is a `SuccessBase`. This is like map for `t`.
    *
    * ''Note:'': This will throw a `java.lang.Throwable` if it is a `FailureBase` and the evaluation of `f` throws a `java.lang.Throwable` instance.
    */
  def recover[W >: V](f: PartialFunction[T, W]): TryBase[T, W]
  /** Applies the given function `f` if this is a `FailureBase`, otherwise returns this if this is a `SuccessBase`. This is like `flatMap` for `t`.
    *
    * ''Note:'': This will throw a `java.lang.Throwable` if it is a `FailureBase` and the evaluation of `f` throws a `java.lang.Throwable` instance.
    */
  def recoverWith[W >: V](f: PartialFunction[T, TryBase[T, W]]): TryBase[T, W]

  /** Applies the given function `f` if this is a `SuccessBase`, otherwise returns `Unit` if this is a `FailureBase`.
    *
    * ''Note:'': This will throw a `java.lang.Throwable` if it is a `SuccessBase` and the evaluation of `f` throws a `java.lang.Throwable` instance.
    */
  def foreach[W](f: V => W): Unit
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
