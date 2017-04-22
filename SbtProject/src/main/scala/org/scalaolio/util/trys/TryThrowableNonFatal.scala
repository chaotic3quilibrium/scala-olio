/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util.trys                                 **
**   Name:      TryThrowableNonFatal.scala                              **
**                                                                      **
** Description:                                                         **
**  Concrete implementation with java.lang.Throwable as the root        **
**  throwable with additional customizations to make contractually and  **
**  behaviorally identical to scala.util.Try                            **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util.trys

import scala.util.control.NonFatal

import org.scalaolio.util.trys.template.{FailureBase, SuccessBase, TryBase, TryObjectBase}

/** Concrete implementation with java.lang.Throwable as the root throwable with additional customizations to make contractually and behaviorally identical to scala.util.Try
  * @author Jim O'Flaherty
  * @since 2.11
  */
object TryThrowableNonFatal extends TryObjectBase[Throwable] {
  def successT[V]: V => TryThrowableNonFatal[V] =
    SuccessThrowableNonFatal(_)

  def failureT[V]: (Throwable, List[(FailureBase[_ <: Throwable, _], Option[java.io.Serializable])]) => TryThrowableNonFatal[V] =
    (t, failureEnclosingContext) =>
      FailureThrowableNonFatal(t, failureEnclosingContext)

  override def isInstanceValid(t: Throwable): Boolean =
    super.isInstanceValid(t) && NonFatal(t)
}

sealed abstract class TryThrowableNonFatal[+V] extends TryBase[Throwable, V] {
  protected val tryObjectBase =
    TryThrowableNonFatal

  def failed: TryBase[Throwable, Throwable]
}

final case class SuccessThrowableNonFatal[+V](override val v: V) extends TryThrowableNonFatal[V] with SuccessBase[Throwable, V] {
  override def filter(p: V => Boolean): TryBase[Throwable, V] =
    try
      super.filter(p)
    catch {
      case NonFatal(e) =>
        tryObjectBase.failureT(e, Nil).asInstanceOf[TryBase[Throwable, V]]
    }

  override def flatMap[W](f: V => TryBase[Throwable, W]): TryBase[Throwable, W] =
    try
      super.flatMap(f)
    catch {
      case NonFatal(e) =>
        tryObjectBase.failureT(e, Nil).asInstanceOf[TryBase[Throwable, W]]
    }

  override def transform[W](fv: V => TryBase[Throwable, W], ft: Throwable => TryBase[Throwable, W]): TryBase[Throwable, W] =
    try
      fv(v)
    catch {
      case NonFatal(e) =>
        tryObjectBase.failureT(e, Nil).asInstanceOf[TryBase[Throwable, W]]
    }


  def failed =
    tryObjectBase.failureT[Throwable](new UnsupportedOperationException("Success.failed"), Nil)
}

final case class FailureThrowableNonFatal[+V](override val t: Throwable, override val enclosingContext: List[(FailureBase[_ <: Throwable, _], Option[java.io.Serializable])] = Nil) extends TryThrowableNonFatal[V] with FailureBase[Throwable, V] {
  override def transform[W](fv: V => TryBase[Throwable, W], ft: Throwable => TryBase[Throwable, W]): TryBase[Throwable, W] =
    try
      ft(t)
    catch {
      case NonFatal(e) =>
        tryObjectBase.failureT(e, enclosingContext).asInstanceOf[TryBase[Throwable, W]]
    }

  override def orElse[W >: V](default: => TryBase[Throwable, W]): TryBase[Throwable, W] =
    try
      default
    catch {
      case NonFatal(e) =>
        tryObjectBase.failureT(e, enclosingContext).asInstanceOf[TryBase[Throwable, W]]
    }

  override def recover[W >: V](f: PartialFunction[Throwable, W]): TryBase[Throwable, W] =
    try
      super.recover(f)
    catch {
      case NonFatal(e) =>
        tryObjectBase.failureT(e, enclosingContext).asInstanceOf[TryBase[Throwable, W]]
    }
  override def recoverWith[W >: V](f: PartialFunction[Throwable, TryBase[Throwable, W]]): TryBase[Throwable, W] =
    try
      super.recoverWith(f)
    catch {
      case NonFatal(e) =>
        tryObjectBase.failureT(e, enclosingContext).asInstanceOf[TryBase[Throwable, W]]
    }


  val failed =
    tryObjectBase.successT(t)
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
