/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util.csv                                  **
**   Name:      Composer.scala                                          **
**                                                                      **
** Description:                                                         **
**  Super simple CSV line and file composer                             **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util.csv

object Composer {
  def toLine(line: List[String]): String = {
    def encode(value: String): String = {
      if ((value.indexOf(',') < 0) && (value.indexOf('"') < 0))
        //no commas or double quotes, so nothing to encode
        value
      else
        //found a comma or a double quote,
        //  so double all the double quotes
        //  and then surround the whole result with double quotes
        "\"" + value.replace("\"", "\"\"") + "\""
    }
    if (line.nonEmpty)
      line.map(encode).mkString(",")
    else
      ""
  }

  def toLines(lines: List[List[String]]): List[String] =
    lines.map(toLine)
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
