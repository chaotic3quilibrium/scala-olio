/* ---------.---------.---------.---------.---------.---------.-------- *\
** Part Of:     Scala Olio API                                          **
** URL:         http://www.scalaolio.org                                **
** File:                                                                **
**   Package:   org.scalaolio.util.csv                                  **
**   Name:      Parser.scala                                            **
**                                                                      **
** Description:                                                         **
**  Super simple CSV line and file parser                               **
**                                                                      **
** License:   GPLv3 license (see end of file for details)               **
** Ownership: Copyright (C) 2016 by Jim O'Flaherty                      **
\* ---------.---------.---------.---------.---------.---------.-------- */
package org.scalaolio.util.csv

object Parser {
  def fromLine(line: String): List[String] = {
    def recursive(
        lineRemaining: String
      , isWithinDoubleQuotes: Boolean
      , valueAccumulator: String
      , accumulator: List[String]
    ): List[String] = {
      if (lineRemaining.isEmpty)
        valueAccumulator :: accumulator
      else
        if (lineRemaining.head == '"')
          if (isWithinDoubleQuotes)
            if (lineRemaining.tail.nonEmpty && lineRemaining.tail.head == '"')
              //escaped double quote
              recursive(lineRemaining.drop(2), isWithinDoubleQuotes = true, valueAccumulator + '"', accumulator)
            else
              //end of double quote pair (ignore whatever's between here and the next comma)
              recursive(lineRemaining.dropWhile(_ != ','), isWithinDoubleQuotes = false, valueAccumulator, accumulator)
          else
            //start of a double quote pair (ignore whatever's in valueAccumulator)
            recursive(lineRemaining.drop(1), isWithinDoubleQuotes = true, "", accumulator)
        else
          if (isWithinDoubleQuotes)
            //scan to next double quote
            recursive(
                lineRemaining.dropWhile(_ != '"')
              , isWithinDoubleQuotes = true
              , valueAccumulator + lineRemaining.takeWhile(_ != '"')
              , accumulator
            )
          else
            if (lineRemaining.head == ',')
              //advance to next field value
              recursive(
                  lineRemaining.drop(1)
                , isWithinDoubleQuotes = false
                , ""
                , valueAccumulator :: accumulator
              )
            else
              //scan to next double quote or comma
              recursive(
                  lineRemaining.dropWhile(char => (char != '"') && (char != ','))
                , isWithinDoubleQuotes = false
                , valueAccumulator + lineRemaining.takeWhile(char => (char != '"') && (char != ','))
                , accumulator
              )
    }
    if (line.nonEmpty)
      recursive(line, isWithinDoubleQuotes = false, "", Nil).reverse
    else
      Nil
  }

  def fromLines(lines: List[String]): List[List[String]] =
    lines.map(fromLine)
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
