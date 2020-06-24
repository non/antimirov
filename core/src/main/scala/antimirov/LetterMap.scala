package antimirov

import java.util.Arrays
import scala.collection.mutable
import scala.reflect.ClassTag

/**
 * keys.length is even
 * vals.length = keys.length / 2
 */
class LetterMap[A](
  private[antimirov] val keys: Array[Char],
  private[antimirov] val vals: Array[A]
) { lhs =>

  override def equals(that: Any): Boolean =
    that match {
      case lm: LetterMap[A] =>
        if (!Arrays.equals(this.keys, lm.keys)) return false
        // $COVERAGE-OFF$
        if (vals.length != lm.vals.length) return false
        // $COVERAGE-ON$
        var i = 0
        while (i < vals.length) {
          if (vals(i) != lm.vals(i)) return false
          i += 1
        }
        true
      case _ =>
        false
    }

  override def hashCode: Int = {
    var n = 0xc001d065
    var i = 0
    while (i < vals.length) {
      n = (n * 84499) + vals(i).hashCode
      i += 1
    }
    Arrays.hashCode(keys) + n
  }

  def iterator: Iterator[((Char, Char), A)] =
    new Iterator[((Char, Char), A)] {
      var i = 0
      def hasNext: Boolean = i < keys.length
      def next(): ((Char, Char), A) = {
        val item = ((keys(i), keys(i + 1)), vals(i >>> 1))
        i += 2
        item
      }
    }

  def items: Iterator[(Char, A)] =
    iterator.flatMap { case ((c1, c2), a) =>
      (c1 to c2).iterator.map(c => (c, a))
    }

  def isEmpty: Boolean = keys.length == 0

  def nonEmpty: Boolean = keys.length != 0

  lazy val size: Int =
    iterator.map { case ((x, y), _) => y - x + 1 }.sum

  def minKeyOption: Option[Char] =
    if (isEmpty) None else Some(keys(0))

  def maxKeyOption: Option[Char] =
    if (isEmpty) None else Some(keys.last)

  def toMap: Map[Char, A] =
    items.toMap

  def merge(rhs: LetterMap[A])(f: (A, A) => A)(implicit ct: ClassTag[A]): LetterMap[A] = {
    val it = LetterSet.diff(lhs.keySet, rhs.keySet)
    val kbuf = mutable.ArrayBuffer.empty[Char]
    val vbuf = mutable.ArrayBuffer.empty[A]
    while (it.hasNext) {
      val diff = it.next
      val (c1, c2) = diff.value
      val v = diff match {
        case Diff.Left((c, _)) => lhs(c)
        case Diff.Both((c, _)) => f(lhs(c), rhs(c))
        case Diff.Right((c, _)) => rhs(c)
      }
      // if our new region is adjacent to our previous region and
      // we're writing the same value, we should extend the previous
      // region rather than writing a new one here.
      if (kbuf.nonEmpty && c1 == (kbuf.last + 1) && vbuf.last == v) {
        kbuf(kbuf.size - 1) = c2
      } else {
        kbuf.append(c1)
        kbuf.append(c2)
        vbuf.append(v)
      }
    }
    new LetterMap(kbuf.toArray, vbuf.toArray)
  }

  def mapValues[B: ClassTag](f: A => B): LetterMap[B] = {
    val kbuf = mutable.ArrayBuffer.empty[Char]
    val vbuf = mutable.ArrayBuffer.empty[B]
    var i = 0
    var j = 0
    while (i < keys.length && j < vals.length) {
      val v = f(vals(j))
      if (kbuf.nonEmpty && keys(i) == (kbuf.last + 1) && v == vbuf.last) {
        kbuf(kbuf.size - 1) = keys(i + 1)
      } else {
        kbuf.append(keys(i))
        kbuf.append(keys(i + 1))
        vbuf.append(v)
      }
      i += 2
      j += 1
    }
    new LetterMap(kbuf.toArray, vbuf.toArray)
  }

  override def toString: String =
    show(_.toString)

  def show(f: A => String): String =
    (0 until keys.length by 2).map { i =>
      val pair = (keys(i), keys(i + 1))
      val ks = LetterSet.rangeRepr(pair)
      val vs = f(vals(i / 2))
      s"[$ks] -> $vs"
    }.mkString("LetterMap(", ", ", ")")

  def ++(rhs: LetterMap[A])(implicit ct: ClassTag[A]): LetterMap[A] =
    merge(rhs)((_, a) => a)

  def keySet: LetterSet =
    new LetterSet(keys)

  def contains(c: Char): Boolean = {
    val n = Arrays.binarySearch(keys, c)
    (n >= 0) || (-(n + 1) % 2 == 1)
  }

  def apply(c: Char): A =
    Arrays.binarySearch(keys, c) match {
      case n if n >= 0 =>
        vals(n / 2)
      case n =>
        val i = -(n + 1)
        if (i % 2 == 1) vals(i / 2) else sys.error("!")
    }

  def get(c: Char): Option[A] =
    Arrays.binarySearch(keys, c) match {
      case n if n >= 0 =>
        Some(vals(n / 2))
      case n =>
        val i = -(n + 1)
        if (i % 2 == 1) Some(vals(i / 2)) else None
    }
}

object LetterMap {

  def empty[A: ClassTag]: LetterMap[A] =
    new LetterMap(new Array[Char](0), new Array[A](0))

  def apply[A: ClassTag](c: Char, a: A): LetterMap[A] =
    apply(LetterSet(c), a)

  def apply[A: ClassTag](keys: LetterSet, a: A): LetterMap[A] = {
    val ks = keys.array
    val vs = Array.fill(ks.length / 2)(a)
    new LetterMap(ks, vs)
  }
}
