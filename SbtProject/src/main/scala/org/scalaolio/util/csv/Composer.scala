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
      line.map(encode(_)).mkString(",")
    else
      ""
  }

  def toLines(lines: List[List[String]]): List[String] =
    lines.map(toLine)
}
