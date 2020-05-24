package antimirov

import java.util.Arrays
import scala.collection.mutable
import scala.collection.immutable.NumericRange

/**
 * LetterSet represents a set of characters.
 *
 * It's logically equivalent to Set[Char] but represents its contents
 * as an array of inclusive character ranges. In the worst case this
 * means it uses 2x as an Array[Char] normally would. In the best case
 * it uses 4 bytes to represent 65,536 characters (this is the set
 * containing all characters).
 *
 * Some facts related to the internal representation:
 *
 *   - array.length is always even
 *   - ranges are inclusive, non-overlapping, and stored in order
 *   - the order of ranges is lowest-to-highest (by start)
 *   - i.e. each range is [start, end] where start <= end
 *   - single elements are represented as [c, c]
 *   - all elements found in the array is are members of the set
 *   - elements not found in the array might also be members
 *
 * This layout means we can use binary search to quickly check for
 * membership or determine where to add new elements. If binary search
 * "finds" a value we know it is already contained in the set. If
 * instead it returns a negative index, we can tell whether the
 * element is contained in a range based on whether the calculated
 * insertion index is even (absent) or odd (present).
 *
 * For example:
 *
 *     let array = [2, 4, 6, 6, 10, 13]
 *     searchFor(1) -> insertion index is 0   -> absent
 *     searchFor(2) -> actual index is 0      -> present
 *     searchFor(3) -> insertion index is 1   -> present
 *     searchFor(5) -> insertion index is 2   -> absent
 *     searchFor(6) -> actual-index is 2 or 3 -> present
 *     searchFor(7) -> insertion index is 4   -> absent
 *     and so on...
 *
 * This class is most efficient when dealing with sets that are
 * represented as a relatively small number of single characters plus
 * some (potentially very large) ranges. These often come up with
 * regular expressions, for example:
 *
 *     /[0-9]/     -> 10 members represented in 4 bytes
 *     /[a-zA-Z]/  -> 52 members represented in 8 bytes
 *     /[^a-zA-Z]/ -> 65,484 members represented in 12 bytes
 *     /./         -> 65,536 members represented in 4 bytes
 *
 * It is less optimal for non-contiguous sets of single characters:
 *
 *     /[aeiou]/   -> 5 members represented in 20 bytes
 *     /[acegikm]/ -> 7 members represented in 28 bytes
 *
 * Similarly, checking for set membership is O(log(n)) where n is the
 * number of contiguous ranges. This makes the implementation less
 * efficient than Set[Char] (where it is O(1)) unless the caller
 * expects this n to be small relative to the number of elements
 * contained. The other operations (e.g. union, intersection) are
 * likely to always be competitive since LetterSet's impementations
 * are O(n) in the number of ranges, whereas Set[Char]'s are O(n) in
 * the number of individual elements. Finally, LetterSet's set
 * complement operator (~) has no direct competitor in Set[Char] since
 * that operation would be very expensive.
 *
 * You could imagine building a data structure that uses a Set[Char]
 * for individual characters and only uses a LetterSet for contiguous
 * ranges. This approach is not used here because the complexity of
 * going back and forth between these representations when characters
 * are added and removed would add considerable complexity to the
 * (already complex) implementation.
 */
class LetterSet(private val array: Array[Char]) { lhs =>

  override def equals(that: Any): Boolean =
    that match {
      case ls: LetterSet => Arrays.equals(array, ls.array)
      case _ => false
    }

  override lazy val hashCode: Int =
    Arrays.hashCode(array)

  override def toString: String =
    array.mkString("LetterSet(", "", ")")
  //override def toString: String = repr

  def escape(c: Char): String =
    Chars.escape(c, Chars.RangeSpecial)

  def repr: String =
    ranges.map {
      case (x, y) if x == y => escape(x)
      case (x, y) => s"${escape(x)}-${escape(y)}"
    }.mkString("[", "", "]")

  lazy val size: Int =
    ranges.map { case (x, y) => y - x + 1 }.sum

  def ranges: Iterator[(Char, Char)] =
    new Iterator[(Char, Char)] {
      var i = 0
      def hasNext: Boolean = i < array.length
      def next(): (Char, Char) = {
        val item = (array(i), array(i + 1))
        i += 2
        item
      }
    }

  def subsetOf(rhs: LetterSet): Boolean = {
    var start = 0
    val it = lhs.ranges
    while (it.hasNext) {
      val (c1, c2) = it.next
      val (pos, ok) = rhs.containsRangeIndex(c1, c2, start)
      if (!ok) return false
      start = pos
    }
    true
  }

  def supersetOf(rhs: LetterSet): Boolean =
    rhs subsetOf lhs

  def partialCompare(rhs: LetterSet): Double =
    (lhs subsetOf rhs, lhs supersetOf rhs) match {
      case (true, true) => 0.0
      case (true, false) => -1.0
      case (false, true) => 1.0
      case (false, false) => Double.NaN
    }

  def iterator: Iterator[Char] =
    ranges.flatMap { case (c1, c2) => (c1 to c2).iterator }

  def forall(f: Char => Boolean): Boolean =
    iterator.forall(f)

  def isEmpty: Boolean =
    array.isEmpty

  def isSingleton: Boolean =
    array.length == 2 && array(0) == array(1)

  def isFull: Boolean =
    this == LetterSet.Full

  def singleValue: Option[Char] =
    if (array.length == 2 && array(0) == array(1)) Some(array(0)) else None

  def minOption: Option[Char] =
    if (array.length == 0) None else Some(array(0))

  def maxOption: Option[Char] =
    if (array.length == 0) None else Some(array.last)

  def apply(c: Char): Boolean = contains(c)

  def intersects(rhs: LetterSet): Boolean =
    LetterSet.diff(lhs, rhs).exists {
      case Diff.Both(_) => true
      case _ => false
    }

  def contains(c: Char): Boolean = {
    val n = Arrays.binarySearch(array, c)
    (n >= 0) || (-(n + 1) % 2 == 1)
  }

  private def containsRangeIndex(c1: Char, c2: Char, start: Int): (Int, Boolean) =
    Arrays.binarySearch(array, start, array.length, c1) match {
      case n1 if n1 < 0 =>
        val i = -(n1 + 1)
        if (i % 2 == 0) return (i, false)
        // we need i == j or n2 == i + 1
        val n2 = Arrays.binarySearch(array, i, array.length, c2)
        val res = if (n2 >= 0) n2 <= (i + 1) else -(n2 + 1) == i
        (Integer.max(0, i - 1), res)
      case n1 => /* n1 >= 0 */
        // we need n2 <= (n1 + 1) or j = (n1 + 1)
        val n2 = Arrays.binarySearch(array, n1, array.length, c2)
        val res = if (n2 >= 0) n2 <= (n1 + 1) else -(n2 + 1) == (n1 + 1)
        (Integer.max(0, n1 - 1), res)
    }

  def containsRange(c1: Char, c2: Char): Boolean =
    containsRangeIndex(c1, c2, 0)._2

  def unary_~ : LetterSet =
    if (this == LetterSet.Empty) LetterSet.Full
    else if (this == LetterSet.Full) LetterSet.Empty
    else {
      val rs = array
      val anchorLeft = (rs(0) == Char.MinValue)
      val anchorRight = (rs.last == Char.MaxValue)
      val delta =
        if (anchorLeft && anchorRight) -2
        else if (anchorLeft || anchorRight) 0
        else 2

      val out = new Array[Char](rs.length + delta)

      var i = 0
      var j = 0
      var mod = 1
      if (anchorLeft) {
        i += 1
      } else {
        out(j) = Char.MinValue
        mod *= -1
        j += 1
      }
      while (i < rs.length && j < out.length) {
        val c = rs(i)
        out(j) = (c + mod).toChar
        mod *= -1
        i += 1
        j += 1
      }
      if (j < out.length) out(j) = Char.MaxValue
      new LetterSet(out)
    }

  def +(c: Char): LetterSet = {
    val n = Arrays.binarySearch(array, c)
    val i = -(n + 1)
    if (n >= 0 || i % 2 == 1) this
    else {
      val b0 = i > 0 && (array(i - 1) + 1 == c)
      val b1 = i < array.length && (array(i) - 1 == c)
      if (b0 && b1) {
        // combine both intervals
        val out = new Array[Char](array.length - 2)
        require(array != null)
        require(out != null)
        System.arraycopy(array, 0, out, 0, i - 1)
        System.arraycopy(array, i + 1, out, i - 1, array.length - i - 1)
        new LetterSet(out)
      } else if (b0 || b1) {
        // replace either earlier bound or later bound
        val out = new Array[Char](array.length)
        System.arraycopy(array, 0, out, 0, array.length)
        val j = if (b0) i - 1 else i
        out(j) = c
        new LetterSet(out)
      } else {
        // add a new interval
        val out = new Array[Char](array.length + 2)
        System.arraycopy(array, 0, out, 0, i)
        out(i) = c
        out(i + 1) = c
        System.arraycopy(array, i, out, i + 2, array.length - i)
        new LetterSet(out)
      }
    }
  }

  def -(c: Char): LetterSet = {
    val n = Arrays.binarySearch(array, c)
    if (n >= 0) {
      val (i, j) = if (n % 2 == 0) (n, n + 1) else (n - 1, n)
      if (array(i) == array(j)) {
        if (array.length == 2) {
          LetterSet.Empty
        } else {
          val out = new Array[Char](array.length - 2)
          System.arraycopy(array, 0, out, 0, i)
          System.arraycopy(array, j + 1, out, i, array.length - j - 1)
          new LetterSet(out)
        }
      } else {
        val out = new Array[Char](array.length)
        System.arraycopy(array, 0, out, 0, array.length)
        val cc = if (n % 2 == 0) c + 1 else c - 1
        out(n) = cc.toChar
        new LetterSet(out)
      }
    } else {
      val i = -(n + 1)
      if (i % 2 == 0) {
        this
      } else {
        val out = new Array[Char](array.length + 2)
        System.arraycopy(array, 0, out, 0, i)
        out(i) = (c - 1).toChar
        out(i + 1) = (c + 1).toChar
        System.arraycopy(array, i, out, i + 2, array.length - i)
        new LetterSet(out)
      }
    }
  }

  def &(rhs: LetterSet): LetterSet = {
    val buf = mutable.ArrayBuffer.empty[Char]
    val xs = lhs.array
    val ys = rhs.array
    def loop(i: Int, j: Int): Unit =
      if (i < xs.length && j < ys.length) {
        val (x0, x1) = (xs(i), xs(i + 1))
        val (y0, y1) = (ys(j), ys(j + 1))
        if (x0 <= y0) {
          if (x1 < y1) {
            if (x1 >= y0) {
              buf.append(y0)
              buf.append(x1)
            }
            loop(i + 2, j)
          } else { // x1 >= y1
            buf.append(y0)
            buf.append(y1)
            loop(i, j + 2)
          }
        } else { // y0 < x0
          if (y1 < x1) {
            if (y1 >= x0) {
              buf.append(x0)
              buf.append(y1)
            }
            loop(i, j + 2)
          } else { // y1 >= x1
            buf.append(x0)
            buf.append(x1)
            loop(i + 2, j)
          }
        }
      } else ()

    loop(0, 0)
    if (buf.isEmpty) LetterSet.Empty else new LetterSet(buf.toArray)
  }

  def |(rhs: LetterSet): LetterSet =
    if      (lhs.isEmpty || rhs.isFull) rhs
    else if (rhs.isEmpty || lhs.isFull) lhs
    else {
      val buf = mutable.ArrayBuffer.empty[Char]
      var i = 0
      val xs = lhs.array
      var j = 0
      val ys = rhs.array

      var start: Char = Char.MaxValue
      var end: Char = Char.MaxValue
      if (xs(0) <= ys(0)) {
        start = xs(0)
        end = xs(1)
        i += 2
      } else {
        start = ys(0)
        end = ys(1)
        j += 2
      }

      while (i < xs.length || j < ys.length) {
        if (i < xs.length && (j >= ys.length || xs(i) <= ys(j))) {
          if (xs(i) <= (end + 1)) {
            end = Integer.max(end.toInt, xs(i + 1).toInt).toChar
          } else {
            buf.append(start)
            buf.append(end)
            start = xs(i)
            end = xs(i + 1)
          }
          i += 2
        } else {
          if (ys(j) <= (end + 1)) {
            end = Integer.max(end.toInt, ys(j + 1).toInt).toChar
          } else {
            buf.append(start)
            buf.append(end)
            start = ys(j)
            end = ys(j + 1)
          }
          j += 2
        }
      }

      buf.append(start)
      buf.append(end)
      new LetterSet(buf.toArray)
    }

  // TODO: could reduce allocations/work by calculating directly
  def --(rhs: LetterSet): LetterSet =
    lhs & (~rhs)

  // TODO: could reduce allocations/work by calculating directly
  def ^(rhs: LetterSet): LetterSet =
    (lhs | rhs) -- (lhs & rhs)
}

object LetterSet {

  def empty: LetterSet =
    Empty

  def apply(c: Char): LetterSet =
    new LetterSet(Array(c, c))

  def apply(xs: Char*): LetterSet =
    xs.foldLeft(Empty)(_ + _)

  def apply(r: NumericRange[Char]): LetterSet =
    if (r.isInclusive) new LetterSet(Array(r.start, r.end))
    else new LetterSet(Array(r.start, (r.end - 1).toChar))

  def apply(xs: Set[Char]): LetterSet =
    xs.foldLeft(Empty)(_ + _)

  val Empty: LetterSet =
    new LetterSet(Array.empty[Char])

  val Full: LetterSet =
    new LetterSet(Array(Char.MinValue, Char.MaxValue))

  def fromIterator(it: Iterator[(Char, Char)]): LetterSet = {
    var i = -1
    val buf = mutable.ArrayBuffer.empty[Char]
    while (it.hasNext) {
      val (c1, c2) = it.next
      if (i > 0 && buf(i) == c1) {
        buf(i) = c2
      } else {
        buf.append(c1)
        buf.append(c2)
        i += 2
      }
    }
    new LetterSet(buf.toArray)
  }

  def union(x: LetterSet, y: LetterSet): LetterSet =
    LetterSet.fromIterator(diff(x, y).map(_.value))

  def diff(x: LetterSet, y: LetterSet): Iterator[Diff[(Char, Char)]] =
    diff(x.ranges, y.ranges)

  def diff(it1: Iterator[(Char, Char)], it2: Iterator[(Char, Char)]): Iterator[Diff[(Char, Char)]] =
    if (!it1.hasNext) it2.map(Diff.Right(_))
    else if (!it2.hasNext) it1.map(Diff.Left(_))
    else new Iterator[Diff[(Char, Char)]] {

      var lhs: (Char, Char) = it1.next
      var rhs: (Char, Char) = it2.next

      def hasNext: Boolean =
        lhs != null || rhs != null

      def next(): Diff[(Char, Char)] =
        if (lhs == null) {
          val res = rhs
          rhs = if (it2.hasNext) it2.next else null
          Diff.Right(res)
        } else if (rhs == null) {
          val res = lhs
          lhs = if (it1.hasNext) it1.next else null
          Diff.Left(res)
        } else {
          val (x1, x2) = lhs
          val (y1, y2) = rhs
          if (x1 < y1) {
            if (x2 < y1) {
              lhs = if (it1.hasNext) it1.next else null
              Diff.Left((x1, x2))
            } else {
              lhs = (y1, x2)
              Diff.Left((x1, (y1 - 1).toChar))
            }
          } else if (y1 < x1) {
            if (y2 < x1) {
              rhs = if (it2.hasNext) it2.next else null
              Diff.Right((y1, y2))
            } else {
              rhs = (x1, y2)
              Diff.Right((y1, (x1 - 1).toChar))
            }
          } else { /* x1 = y1 */
            val end = Integer.min(x2.toInt, y2.toInt).toChar
            lhs =
              if (x2 > end) ((end + 1).toChar, x2)
              else if (it1.hasNext) it1.next
              else null
            rhs =
              if (y2 > end) ((end + 1).toChar, y2)
              else if (it2.hasNext) it2.next
              else null
            Diff.Both((x1, end))
          }
        }
    }
}

sealed abstract class Diff[A](val value: A) {

  def isLeft: Boolean =
    this match {
      case Diff.Left(_) => true
      case _ => false
    }

  def isBoth: Boolean =
    this match {
      case Diff.Both(_) => true
      case _ => false
    }

  def isRight: Boolean =
    this match {
      case Diff.Right(_) => true
      case _ => false
    }
}

object Diff {
  case class Left[A](a: A) extends Diff(a)
  case class Both[A](a: A) extends Diff(a)
  case class Right[A](a: A) extends Diff(a)
}
