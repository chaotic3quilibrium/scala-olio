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
}
