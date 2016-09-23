/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util.trys.template                        **
**   Name:      SuccessBase.scala                                       **
**                                                                      **
** Description:                                                         **
**  Hidden implementation trait for Success                             **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util.trys.template

/** Hidden implementation trait for Success
  * @author Jim O'Flaherty
  * @since 2.11
  */
trait SuccessBase[T <: Throwable, +V] extends TryBase[T, V] {
  //protected def tryObjectBase: TryObjectBase[T]

  val isSuccess =
    true

  //def v: V


  val isFailure =
    !isSuccess

  //def t: T


  def get =
    v

  def getOrElse[W >: V](default: => W): W =
    get

  def toOption: Option[V] =
    Some(get)


  def filter(p: V => Boolean): TryBase[T, V] =
    if (p(v))
      this
    else
      tryObjectBase.failureT[V](TryBase.FailedPreconditionTryBase(s"filter predicate does not hold for v [$v]").asInstanceOf[T], Nil)

  def flatMap[W](f: V => TryBase[T, W]): TryBase[T, W] =
    f(v)

  def flatten[W](implicit ev: V <:< TryBase[T, W]): TryBase[T, W] =
    this.asInstanceOf[TryBase[T, W]]

  def map[W](f: V => W): TryBase[T, W] =
    tryObjectBase(f(v))

  def transform[W](fv: V => TryBase[T, W], ft: T => TryBase[T, W]): TryBase[T, W] =
    fv(v)


  def orElse[W >: V](default: => TryBase[T, W]): TryBase[T, W] =
    this.asInstanceOf[TryBase[T, W]]

  def recover[W >: V](f: PartialFunction[T, W]): TryBase[T, W] =
    this.asInstanceOf[TryBase[T, W]]

  def recoverWith[W >: V](f: PartialFunction[T, TryBase[T, W]]): TryBase[T, W] =
    this.asInstanceOf[TryBase[T, W]]


  def foreach[W](f: V => W): Unit =
    f(v)
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
