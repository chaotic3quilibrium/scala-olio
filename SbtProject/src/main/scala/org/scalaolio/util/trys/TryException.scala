/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util.trys                                 **
**   Name:      TryException.scala                                      **
**                                                                      **
** Description:                                                         **
**  Concrete implementation with java.lang.Exception as the root        **
**  throwable                                                           **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util.trys

import org.scalaolio.util.trys.template.{FailureBase, SuccessBase, TryBase, TryObjectBase}

/** Concrete implementation with java.lang.Exception as the root throwable
  * @author Jim O'Flaherty
  * @since 2.11
  */
object TryException extends TryObjectBase[Exception] {
  override val classT =
    new Exception().getClass

  def successT[V]: V => TryException[V] =
    SuccessException(_)

  def failureT[V]: (Exception, List[(FailureBase[_ <: Throwable, _], Option[java.io.Serializable])]) => TryException[V] =
    (t, failureEnclosingContext) =>
      FailureException(t, failureEnclosingContext)
}

sealed abstract class TryException[+V] extends TryBase[Exception, V] {
  protected val tryObjectBase =
    TryException
}

final case class SuccessException[+V](override val v: V) extends TryException[V] with SuccessBase[Exception, V]

final case class FailureException[+V](override val t: Exception, override val enclosingContext: List[(FailureBase[_ <: Throwable, _], Option[java.io.Serializable])] = Nil) extends TryException[V] with FailureBase[Exception, V]
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
