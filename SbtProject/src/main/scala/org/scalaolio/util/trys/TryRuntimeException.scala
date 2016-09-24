/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util.trys                                 **
**   Name:      TryRuntimeException.scala                               **
**                                                                      **
** Description:                                                         **
**  Concrete implementation with java.lang.RuntimeException as the root **
**  throwable                                                           **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util.trys

import org.scalaolio.util.trys.template.{FailureBase, SuccessBase, TryBase, TryObjectBase}

/** Concrete implementation with java.lang.RuntimeException as the root throwable
  * @author Jim O'Flaherty
  * @since 2.11
  */
object TryRuntimeException extends TryObjectBase[RuntimeException] {
  override val classT =
    new RuntimeException().getClass

  def successT[V]: V => TryRuntimeException[V] =
    SuccessRuntimeException(_)

  def failureT[V]: (RuntimeException, List[(FailureBase[_ <: Throwable, _], Option[java.io.Serializable])]) => TryRuntimeException[V] =
    (t, failureEnclosingContext) =>
      FailureRuntimeException(t, failureEnclosingContext)
}

sealed abstract class TryRuntimeException[+V] extends TryBase[RuntimeException, V] {
  protected val tryObjectBase =
    TryRuntimeException
}

final case class SuccessRuntimeException[+V](override val v: V) extends TryRuntimeException[V] with SuccessBase[RuntimeException, V]

final case class FailureRuntimeException[+V](override val t: RuntimeException, override val enclosingContext: List[(FailureBase[_ <: Throwable, _], Option[java.io.Serializable])] = Nil) extends TryRuntimeException[V] with FailureBase[RuntimeException, V]
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
