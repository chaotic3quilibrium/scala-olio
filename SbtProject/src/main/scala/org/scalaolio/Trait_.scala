/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio                                           **
**   Name:      Trait_.scala                                            **
**                                                                      **
** Description:                                                         **
**  Catch-all for things related to Scala's trait                       **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2014 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio

import scala.reflect.runtime.universe.{Symbol,TypeTag}

object Trait_ {
  def fetchSealedTraitDescendantsViaTraitSymbol(symbol: Symbol): Option[Set[Symbol]] = {
    val internal = symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol]
    if (internal.isSealed)
      Some(internal.sealedDescendants.map(_.asInstanceOf[Symbol]) - symbol)
    else None
  }

  //TODO: must research doing the sealed trait work via regular class definitions, avoiding 2.10+ reflection mechanism
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
