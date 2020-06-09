package antimirov

import scala.collection.mutable

final class Index {
  private var nextId: Int = 0
  def next(): Int = {
    val n = nextId
    nextId += 1
    n
  }
}
