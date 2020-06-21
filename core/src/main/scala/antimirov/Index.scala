package antimirov

import scala.collection.mutable

sealed class Index {
  private var nextId: Int = 0
  def next(): Int = {
    val n = nextId
    nextId += 1
    n
  }
}

final class ValueIndex[A] extends Index {

  private val valueToIndex =
    mutable.Map.empty[A, Int]

  def initialize(as: Iterable[A]): Unit =
    as.foreach(indexOf)

  def indexOf(a: A): Int =
    valueToIndex.getOrElseUpdate(a, {
      val idx = this.next()
      valueToIndex(a) = idx
      idx
    })
}
