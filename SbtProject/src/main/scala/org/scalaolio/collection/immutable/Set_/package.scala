package org.scalaolio.collection.immutable

package object Set_ {
  def containsAll[T](set: Set[T], subset: Set[T]): Boolean =
    subset.subsetOf(set)

  def containsAll[T](set: Set[T], args: T*): Boolean =
    containsAll(set, args.toSet)

  implicit class RichSet[T](val set: Set[T]) extends AnyVal {
      def containsAll(subset: Set[T]): Boolean =
        Set_.containsAll(set, subset)

      def containsAll(args: T*): Boolean =
        Set_.containsAll(set, args.toSet)
  }
}
