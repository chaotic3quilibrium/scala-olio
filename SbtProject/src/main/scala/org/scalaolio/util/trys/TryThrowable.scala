/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util.trys                                 **
**   Name:      TryThrowable.scala                                      **
**                                                                      **
** Description:                                                         **
**  Concrete implementation with java.lang.Throwable as the root        **
**  throwable                                                           **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util.trys

import org.scalaolio.util.trys.template.{FailureBase, SuccessBase, TryBase, TryObjectBase}

/** Concrete implementation with java.lang.Throwable as the root throwable
  * @author Jim O'Flaherty
  * @since 2.11
  */
object TryThrowable extends TryObjectBase[Throwable] {
  def successT[V]: V => TryThrowable[V] =
    SuccessThrowable(_)

  def failureT[V]: (Throwable, List[(FailureBase[Throwable, Any], Option[Serializable])]) => TryThrowable[V] =
    (t, failureEnclosingContext) =>
      FailureThrowable(t, failureEnclosingContext)
}

sealed abstract class TryThrowable[+V] extends TryBase[Throwable, V] {
  protected val tryObjectBase =
    TryThrowable
}

final case class SuccessThrowable[+V](override val v: V) extends TryThrowable[V] with SuccessBase[Throwable, V]

final case class FailureThrowable[+V](override val t: Throwable, override val enclosingContext: List[(FailureBase[Throwable, Any], Option[Serializable])] = Nil) extends TryThrowable[V] with FailureBase[Throwable, V]
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
