package org.scalaolio.collection.immutable

package object List_ {
  def filterDupes[A](items: List[A]): (List[A], List[(A, Int)]) = {
    def recursive(remaining: List[A], index: Int, accumulator: (List[A], List[(A, Int)])): (List[A], List[(A, Int)]) = {
      if (remaining.isEmpty)
        accumulator
      else
        recursive(
            remaining.tail
          , index + 1
          , if (accumulator._1.contains(remaining.head))
              (accumulator._1, (remaining.head, index) :: accumulator._2)
            else
              (remaining.head :: accumulator._1, accumulator._2)
        )
    }
    val (distinct, dupes) = recursive(items, 0, (Nil, Nil))
    (distinct.reverse, dupes.reverse)
  }

  implicit class RichList[A](val value: List[A]) extends AnyVal {
    def filterDupes: (List[A], List[(A, Int)]) =
      List_.filterDupes(value)
  }
}
