/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.java.lang                                 **
**   Name:      AutoCloseable_.scala                                    **
**                                                                      **
** Description:                                                         **
**  Implements the ARM (Automatic Resource Management) pattern          **
**  implemented in Java 7 (1.7). Substitutes exception and null prone   **
**  Java classes and methods with Scala artifacts (like Try and Option) **
**  which play much more consistently with other Scala idioms           **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.java.lang

import scala.util.Try

/** Implements the ARM (Automatic Resource Management) pattern
 *  implemented in Java 7 (1.7). Substitutes exception and null prone
 *  Java classes and methods with Scala artifacts (like Try and Option)
 *  which play much more consistently with other Scala idioms
 */
object AutoCloseable_ {
  def using[A <: AutoCloseable, R](instantiateAutoCloseable: () => A)(transfer: A => Try[R]): Try[R] =
    Try(instantiateAutoCloseable()).flatMap(autoCloseable => try transfer(autoCloseable) finally autoCloseable.close())
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
