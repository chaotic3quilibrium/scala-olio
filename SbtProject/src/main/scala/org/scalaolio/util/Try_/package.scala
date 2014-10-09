/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util.Try_                                 **
**   Name:      package.scala                                           **
**                                                                      **
** Description:                                                         **
**  Enables Try to be used even when the method does not return         **
**  anything in the event of success (i.e. is a mutator)                **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2014 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util

/** This package object serves to ease Scala interactions with mutating
 *  methods. Used extensively in the java.io related packages.
 */
package object Try_ {
  /** Placeholder type for when a mutator method does not intend to
   *  return anything with a Success, but needs to be able to return a
   *  Failure with an unthrown Exception.
   */
  sealed case class CompletedNoException private[Try_] ()

  /** Placeholder instance for when a mutator method needs to indicate
   *  success via Success(completedNoExceptionSingleton)
   */
  val completedNoExceptionSingleton = new CompletedNoException()
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
