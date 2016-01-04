/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util                                      **
**   Name:      EnumerationDecorated.scala                              **
**                                                                      **
** Description:                                                         **
**  DIY Scala Enumeration (closest possible Java Enum equivalent with   **
**  guaranteed pattern matching exhaustiveness checking) with extended  **
**  attributes within a decoration.                                     **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util

/**
 *
 */
trait EnumerationDecorated extends Enumeration {
  /**
   *
   */
  type Decoration <: DecorationBase

  /**
   *
   */
  protected lazy val orderedSet: List[Member] = decorationOrderedSet.map(_.member)

  /**
   *
   */
  trait MemberDecorated extends MemberBase {
    /**
     *
     */
    self: Member =>

    /**
     *
     *  @return
     */
    def decoration: Decoration = decorationByMember(this)
  }

  /**
   *
   */
  val decorationOrderedSet: List[Decoration]

  /**
   *
   */
  lazy val decorationByMember: Map[Member, Decoration] =
    decorationOrderedSet.map(decoration => (decoration.member, decoration)).toMap

  /**
   *
   */
  trait DecorationBase {
    /**
     *
     */
    self: Decoration =>

    /**
     *
     *  @return
     */
    def member: Member
  }
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
