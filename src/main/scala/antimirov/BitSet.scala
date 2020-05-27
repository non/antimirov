package antimirov

import java.util.Arrays

/**
 * Fast BitSet implementation (based on Spire's).
 *
 * This mutable bitset is just intended to be a little bit faster than
 * Scala's, and to support accessing its internals.
 *
 * Size is the number of elements the bitset represents (with 1 or 0);
 * the size is static and cannot be expanded.
 */
case class BitSet(size: Int, array: Array[Int]) { lhs =>

  override def toString: String =
    (0 until size).filter(apply(_)).map(_.toString).mkString("BitSet(", ", ", ")")

  def apply(n: Int): Boolean =
    ((array(n >>> 5) >>> (n & 31)) & 1) == 1

  def intersects(rhs: BitSet): Boolean = {
    var i = 0
    while (i < array.length) {
      if ((array(i) & rhs.array(i)) != 0) return true
      i += 1
    }
    false
  }

  def |(rhs: BitSet): BitSet = {
    val newArray = Arrays.copyOf(lhs.array, lhs.array.length)
    val bitset = BitSet(lhs.size, newArray)
    bitset |= rhs
    bitset
  }

  def &(rhs: BitSet): BitSet = {
    val newArray = Arrays.copyOf(lhs.array, lhs.array.length)
    val bitset = BitSet(lhs.size, newArray)
    bitset &= rhs
    bitset
  }

  def +=(n: Int): Unit =
    array(n >>> 5) |= (1 << (n & 31))

  def -=(n: Int): Unit =
    array(n >>> 5) &= ~(1 << (n & 31))

  def update(n: Int, b: Boolean): Unit =
    if (b) this += n else this -= n

  def |=(rhs: BitSet): Unit = {
    var i = 0
    val arr = rhs.array
    while (i < arr.length) {
      array(i) |= arr(i)
      i += 1
    }
  }

  def &=(rhs: BitSet): Unit = {
    var i = 0
    val arr = rhs.array
    while (i < arr.length) {
      array(i) &= arr(i)
      i += 1
    }
  }

  def copy(): BitSet =
    BitSet(size, Arrays.copyOf(array, array.length))

  def clear(): Unit = {
    var i = 0
    while (i < array.length) {
      array(i) = 0
      i += 1
    }
  }
}

object BitSet {
  def empty(size: Int): BitSet =
    new BitSet(size, new Array[Int]((size + 31) >>> 5))

  def apply(size: Int, ns: Iterable[Int]): BitSet = {
    val bitset = BitSet.empty(size)
    ns.foreach(bitset += _)
    bitset
  }
}
