package antimirov

import java.util.Arrays

/**
 * Fast BitSet implementation (based on Spire's).
 *
 * This mutable bitset is just intended to be a little bit faster than
 * Scala's, and to support accessing its internals. It's not really
 * general purpose -- many affordances that aren't needed have been
 * left off.
 *
 * MaxSize is the number of elements the bitset represents (with 1 or 0);
 * the maxSize is static and cannot be expanded.
 */
case class BitSet(maxSize: Int, array: Array[Int]) { lhs =>

  def apply(n: Int): Boolean =
    ((array(n >>> 5) >>> (n & 31)) & 1) == 1

  def size: Int = {
    var n = 0
    var i = 0
    while (i < array.length) {
      n += Integer.bitCount(array(i))
      i += 1
    }
    n
  }

  override def toString: String =
    (0 until maxSize).filter(apply(_)).map(_.toString).mkString("BitSet(", ", ", ")")

  override def equals(that: Any): Boolean =
    that match {
      case BitSet(ms, arr) if maxSize == ms => Arrays.equals(array, arr)
      case _ => false
    }

  override def hashCode: Int =
    maxSize + Arrays.hashCode(array)

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
    val bitset = BitSet(lhs.maxSize, newArray)
    bitset |= rhs
    bitset
  }

  def &(rhs: BitSet): BitSet = {
    val newArray = Arrays.copyOf(lhs.array, lhs.array.length)
    val bitset = BitSet(lhs.maxSize, newArray)
    bitset &= rhs
    bitset
  }

  def +=(n: Int): Unit =
    array(n >>> 5) |= (1 << (n & 31))

  def -=(n: Int): Unit =
    array(n >>> 5) &= ~(1 << (n & 31))

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
    BitSet(maxSize, Arrays.copyOf(array, array.length))

  def clear(): Unit = {
    var i = 0
    while (i < array.length) {
      array(i) = 0
      i += 1
    }
  }
}

object BitSet {
  def empty(maxSize: Int): BitSet =
    new BitSet(maxSize, new Array[Int]((maxSize + 31) >>> 5))

  def apply(maxSize: Int, ns: Iterable[Int]): BitSet = {
    val bitset = BitSet.empty(maxSize)
    ns.foreach(bitset += _)
    bitset
  }
}
