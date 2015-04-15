package org.scalaolio.collection.immutable

package object List_ {
  def filterDupes[A](items: List[A]): (List[A], List[A]) = {
    def recursive(remaining: List[A], accumulator: (List[A], List[A])): (List[A], List[A]) = {
      if (remaining.isEmpty)
        accumulator
      else
        if (accumulator._1.contains(remaining.head))
          recursive(remaining.tail, (accumulator._1, remaining.head :: accumulator._2))
        else
          recursive(remaining.tail, (remaining.head :: accumulator._1, accumulator._2))
    }
    val (distinct, dupes) = recursive(items, (Nil, Nil))
    (distinct.reverse, dupes.reverse)
  }

  implicit class RichList[A](val value: List[A]) extends AnyVal {
    def filterDupes: (List[A], List[A]) =
      List_.filterDupes(value)
  }
}
