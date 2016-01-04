/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.gis.spatial.surface2d.entity              **
**   Name:      Direction.scala                                         **
**                                                                      **
** Description:                                                         **
**  Simple Enumeration to define the 8 standard geospatial directions   **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.gis.spatial.surface2d.entity

import org.scalaolio.util.Enumeration

import scala.reflect.runtime.universe.{TypeTag, typeTag}

object Direction extends Enumeration {
  case object NORTH extends Member
  case object NORTH_EAST extends Member
  case object EAST extends Member
  case object SOUTH_EAST extends Member
  case object SOUTH extends Member
  case object SOUTH_WEST extends Member
  case object WEST extends Member
  case object NORTH_WEST extends Member

  protected val orderedSet: List[Member] = List(NORTH, NORTH_EAST, EAST, SOUTH_EAST, SOUTH, SOUTH_WEST, WEST, NORTH_WEST)
  val orderedSetOrthogonalOnly: List[Member] = List(NORTH, EAST, SOUTH, WEST)
  val orderedSetDiagonalOnly: List[Member] = List(NORTH_EAST, SOUTH_EAST, SOUTH_WEST, NORTH_WEST)

  sealed trait Member extends MemberBase
  override def typeTagMember: TypeTag[_] = typeTag[Member]
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
