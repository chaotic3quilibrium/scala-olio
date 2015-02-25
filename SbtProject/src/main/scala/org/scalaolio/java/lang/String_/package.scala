package org.scalaolio.java.lang

import scala.annotation.tailrec

package object String_ {
  def indexesOf(source: String, target: String, index: Int = 0, withinOverlaps: Boolean = false): List[Int] = {
    @tailrec
    def recursive(indexTarget: Int, accumulator: List[Int]): List[Int] = {
      val position = source.indexOf(target, indexTarget)
      if (position == -1) accumulator
      else
        recursive(position + (if (withinOverlaps) 1 else target.size), position :: accumulator)
    }
    recursive(index, Nil).reverse
  }

  def splitLiterally(string: String, separator: String): List[String] = {
    @tailrec
    def recursive(remaining: String, accumulator: List[String]): List[String] = {
      if (remaining.isEmpty)
        accumulator
      if (separator.isEmpty)
        remaining :: accumulator
      else {
        val index = remaining.indexOf(separator)
        if (index == -1)
          remaining :: accumulator
        else {
          recursive(remaining.drop(index + separator.size), remaining.take(index) :: accumulator)
        }
      }
    }
    recursive(string, Nil).reverse
  }

  def spanSansSeparator(string: String, separator: String): (String, String) = {
    val index = string.indexOf(separator)
    if (index > -1)
      (string.take(index), string.drop(index + separator.size))
    else
      (string, "")
  }

  implicit class RichString(val value: String) extends AnyVal {
    def indexesOf(target: String, index: Int = 0, withinOverlaps: Boolean = false): List[Int] =
      String_.indexesOf(value, target, index, withinOverlaps)

    def splitLiterally(separator: String): List[String] =
      String_.splitLiterally(value, separator)

    def spanSansSeparator(separator: String): (String, String) =
      String_.spanSansSeparator(value, separator)
  }
}
