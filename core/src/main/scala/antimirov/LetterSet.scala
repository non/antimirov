package antimirov

import java.util.Arrays
import scala.collection.immutable.NumericRange
import scala.collection.mutable

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
 *   - all elements found in the array are members of the set
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
 * ranges. This approach is not used here because the details of
 * going back and forth between these representations when characters
 * are added and removed would add considerable complexity to the
 * (already complex) implementation.
 */
class LetterSet(private[antimirov] val array: Array[Char]) { lhs =>

  /**
   * Java universal equality.
   */
  override def equals(that: Any): Boolean =
    that match {
      case ls: LetterSet => Arrays.equals(array, ls.array)
      case _ => false
    }

  def rev[A](xs: List[A]): List[A] =
    xs.reverse

  /**
   * Hash code.
   *
   * Since the hash code is expensive to compute we memoize the result.
   */
  override lazy val hashCode: Int =
    Arrays.hashCode(array)

  /**
   * Java toString method, using regex character class syntax.
   */
  override def toString: String =
    if (isFull) {
      "."
    } else if (size == 1) {
      val c = array(0)
      Chars.escape(c, Chars.Special)
    } else {
      // TODO: could avoid allocation for ~this
      val (prefix, cs) = if (size <= 32768) ("[", this) else ("[^", ~this)
      cs.ranges.map(LetterSet.rangeRepr).mkString(prefix, "", "]")
    }

  /**
   * Cardinality of the set of characters.
   *
   * This size represents the number of characters matched, not the number of
   * character groups or anything else corresponding to its internal structure.
   */
  lazy val size: Int = {
    var i = 0
    var sum = 0
    while (i < array.length) {
      sum += array(i + 1) - array(i) + 1
      i += 2
    }
    sum
  }

  /**
   * Iterate across each character range in the set.
   */
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

  /**
   * Determine if this set is a subset of the given set.
   *
   * Also returns true if the sets are equal.
   */
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

  /**
   * Determine if this set is a superset of the given set.
   *
   * Also returns true if the sets are equal.
   */
  def supersetOf(rhs: LetterSet): Boolean =
    rhs subsetOf lhs

  /**
   * Compare the given sets using a partial ordering based on set relations.
   *
   * Returns:
   *  *  0.0 if the sets are equal
   *  * -1.0 if `lhs` is a subset of `rhs`
   *  *  1.0 if `lhs` is a superset of `rhs`
   *  *  NaN otherwise
   */
  def partialCompare(rhs: LetterSet): Double =
    (lhs subsetOf rhs, lhs supersetOf rhs) match {
      case (true, true) => 0.0
      case (true, false) => -1.0
      case (false, true) => 1.0
      case (false, false) => Double.NaN
    }

  /**
   * Iterate across every character in the set.
   *
   * Note that this can be very expensive -- it is mostly useful for testing or
   * working interactively. Generally other operations are preferable.
   */
  def iterator: Iterator[Char] =
    ranges.flatMap { case (c1, c2) => (c1 to c2).iterator }

  /**
   * Evaluate a predicate for every character in the set.
   *
   * Note that this can be very expensive -- it is mostly useful for testing or
   * working interactively. Generally other operations are preferable.
   */
  def forall(f: Char => Boolean): Boolean =
    iterator.forall(f)

  /**
   * Test if the set is empty.
   */
  def isEmpty: Boolean =
    array.length == 0

  /**
   * Test if the set contains exactly one character.
   */
  def isSingleton: Boolean =
    array.length == 2 && array(0) == array(1)

  /**
   * Test if the set is non-empty.
   */
  def nonEmpty: Boolean =
    array.length != 0

  /**
   * Test if the set contains all characters.
   */
  def isFull: Boolean =
    this == LetterSet.Full

  /**
   * For single-valued sets return that single value.
   */
  def singleValue: Option[Char] =
    if (array.length == 2 && array(0) == array(1)) Some(array(0)) else None

  /**
   * For non-empty sets return its smallest element.
   */
  def minOption: Option[Char] =
    if (array.length == 0) None else Some(array(0))

  /**
   * For non-empty sets return its largest element.
   */
  def maxOption: Option[Char] =
    if (array.length == 0) None else Some(array.last)

  /**
   * Test if the set contains the given character.
   *
   * This method is an alias for the contains method.
   */
  def apply(c: Char): Boolean = contains(c)

  /**
   * Access elements of the set by positional index.
   *
   * Requires that 0 <= index < size.
   */
  def get(index: Int): Char =
    if (index < 0 || size <= index) {
      sys.error(s"invalid index: $index")
    } else {
      var i = index
      val it = ranges
      while (it.hasNext) {
        val (c1, c2) = it.next
        val span = c2 - c1 + 1
        if (i < span) return (c1 + i).toChar
        i -= span
      }
      // $COVERAGE-OFF$
      sys.error(s"impossible: exhausted ranges with i=$i")
      // $COVERAGE-ON$
    }

  /**
   * Returns whether the given sets contain any characters in common.
   */
  def intersects(rhs: LetterSet): Boolean =
    LetterSet.diff(lhs, rhs).exists {
      case Diff.Both(_) => true
      case _ => false
    }

  /**
   * Returns whether the set contains the given character.
   */
  def contains(c: Char): Boolean = {
    val n = Arrays.binarySearch(array, c)
    (n >= 0) || (-(n + 1) % 2 == 1)
  }

  /**
   * Determine if a given range is contained, starting at the given index.
   *
   * The `start parameter is used to optimize the `subsetOf` method, which can
   * narrow the section of the array to be searched as we move through it.
   */
  private def containsRangeIndex(c1: Char, c2: Char, start: Int): (Int, Boolean) =
    Arrays.binarySearch(array, start, array.length, c1) match {
      case n1 if n1 < 0 =>
        // i is the insert position for n1. we need i to be odd, or
        // else we're not inserting between a range, which means the
        // character is not included. if i is odd then it corresponds
        // to the index of the right boundary.
        val i = -(n1 + 1)
        if (i % 2 == 0) return (i, false)

        // j is the insert position for n2. we need i == j.
        val n2 = Arrays.binarySearch(array, i, array.length, c2)
        val j = if (n2 < 0) -(n2 + 1) else n2
        (Integer.max(0, i - 1), i == j)
      case n1 => /* n1 >= 0 */
        val res = if (n1 % 2 == 1) {
          // if n1 is the right boundary, then we need n2 to be
          // exactly n1 or else it is not contained in the range.
          val n2 = Arrays.binarySearch(array, n1, array.length, c2)
          n2 == n1
        } else {
          // if n1 is the left boundary, then we need n2 to be either
          // n1, (n1 + 1), or else inserted between them.
          val n2 = Arrays.binarySearch(array, n1, array.length, c2)
          if (n2 < 0) {
            // if n2 is being inserted, we require it to be inserted
            // between the left boundary (n1) and the right boundary
            // (n1 + 1).
            val j = -(n2 + 1)
            j == (n1 + 1)
          } else {
            // if n2 is present, it can be either the left boundary
            // (n1) or the right boundary (n1 + 1).
            n2 == n1 || n2 == (n1 + 1)
          }
        }
        (n1, res)
    }

  def containsRange(c1: Char, c2: Char): Boolean =
    containsRangeIndex(c1, c2, 0)._2

  /**
   * Return the complement of this set.
   *
   * The complement accepts the characters that this set rejects, and rejects
   * the set that this set accepts.
   */
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

  /**
   * Add a character to this set.
   */
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

  /**
   * Remove a character to this set.
   */
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

  /**
   * Intersect the given sets.
   *
   * The result accepts characters accepted by both sets.
   */
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

  /**
   * Union the given sets.
   *
   * The result accepts characters accepted by either set.
   */
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
      var last: Char = Char.MaxValue
      if (xs(0) <= ys(0)) {
        start = xs(0)
        last = xs(1)
        i += 2
      } else {
        start = ys(0)
        last = ys(1)
        j += 2
      }

      while (i < xs.length || j < ys.length) {
        if (i < xs.length && (j >= ys.length || xs(i) <= ys(j))) {
          if (xs(i) <= (last + 1)) {
            last = Integer.max(last.toInt, xs(i + 1).toInt).toChar
          } else {
            buf.append(start)
            buf.append(last)
            start = xs(i)
            last = xs(i + 1)
          }
          i += 2
        } else {
          if (ys(j) <= (last + 1)) {
            last = Integer.max(last.toInt, ys(j + 1).toInt).toChar
          } else {
            buf.append(start)
            buf.append(last)
            start = ys(j)
            last = ys(j + 1)
          }
          j += 2
        }
      }

      buf.append(start)
      buf.append(last)
      new LetterSet(buf.toArray)
    }

  /**
   * Set difference.
   *
   * The result accepts characters which `lhs` accepts and `rhs` rejects.
   *
   * (x -- y) is equivalent to (x & ~y).
   */
  def --(rhs: LetterSet): LetterSet =
    //lhs & (~rhs)
    if (rhs.isEmpty) {
      lhs
    } else if (rhs.isFull) {
      LetterSet.empty
    } else {
      val buf = mutable.ArrayBuffer.empty[Char]
      var i = 0
      val xs = lhs.array
      var j = 0
      val ys = rhs.array
      while (i < xs.length) {
        var x1 = xs(i)
        var x2 = xs(i + 1)
        // we will step through ys, using each mask to chip away at (x1, x2)
        // where needed and writing into buf when we are sure no future masks
        // can affect (x1, x2). we increment j only when we are sure we are done
        // with a mask.
        while (j < ys.length && ys(j) <= x2) {
          val y1 = ys(j)
          val y2 = ys(j + 1)
          if (y2 < x1) {
            // y1 <= y2 < x1 <= x2; mask has no effect (done)
            j += 2
          } else if (x1 < y1) {
            // x1 < y1; y1 <= x2; y1 <= y2
            if (x2 <= y2) {
              // x1 < y1 <= x2 <= y2; mask removes the back (not done)
              x2 = (y1 - 1).toChar
            } else {
              // x1 < y1 <= y2 < x2; mask removes the middle (done)
              buf.append(x1)
              buf.append((y1 - 1).toChar)
              x1 = (y2 + 1).toChar
              j += 2
            }
          } else {
            // y1 <= x1 <= x2; y1 <= y2
            if (x2 <= y2) {
              // y1 <= x1 <= x2 <= y2; mask removes everything (not done)
              // signal that we need to skip the append with x1 > x2
              x1 = Char.MaxValue
              x2 = Char.MinValue
            } else {
              // y1 <= x1 <= y2 < x2; mask removes the front (done)
              x1 = (y2 + 1).toChar
              j += 2
            }
          }
        }
        // in some cases we need to "skip" the append. we use x1 > x2 (which is
        // normally illegal) as a signal to do this skipping.
        if (x1 <= x2) {
          buf.append(x1)
          buf.append(x2)
        }
        i += 2
      }
      new LetterSet(buf.toArray)
    }

  /**
   * Exclusive-OR operator.
   *
   * The result accepts characters that are accepted by only one of the sets
   * (but not both).
   *
   * (x ^ y) is equivalent to (x | y) -- (x & y)
   */
  def ^(rhs: LetterSet): LetterSet =
    //(lhs | rhs) -- (lhs & rhs)
    if      (lhs.isEmpty) rhs
    else if (rhs.isEmpty) lhs
    else if (lhs.isFull) ~rhs
    else if (rhs.isFull) ~lhs
    else {
      val buf = mutable.ArrayBuffer.empty[Char]

      def appendAll(xs: List[(Char, Char)]): Unit =
        xs match {
          case (c1, c2) :: rest =>
            buf.append(c1)
            buf.append(c2)
            appendAll(rest)
          case Nil =>
            ()
        }

      def loop(xs: List[(Char, Char)], ys: List[(Char, Char)]): Unit =
        (xs, ys) match {
          case ((x1, x2) :: xt, (y1, y2) :: yt) =>
            if (x1 < y1) {
              if (x2 < y1) {
                if ((x2 + 1) == y1) {
                  // try to make a larger structure
                  loop((x1, y2) :: yt, xt)
                } else {
                  buf.append(x1)
                  buf.append(x2)
                  loop(xt, ys)
                }
              } else {
                buf.append(x1)
                buf.append((y1 - 1).toChar)
                if (x2 < y2) {
                  loop(xt, ((x2 + 1).toChar, y2) :: yt)
                } else if (x2 == y2) {
                  loop(xt, yt)
                } else { // y2 < x2
                  loop(((y2 + 1).toChar, x2) :: xt, yt)
                }
              }
            } else if (y1 < x1) {
              loop(ys, xs)
            } else {
              if (x2 < y2) {
                loop(xt, ((x2 + 1).toChar, y2) :: yt)
              } else if (y2 < x2) {
                loop(((y2 + 1).toChar, x2) :: xt, yt)
              } else {
                loop(xt, yt)
              }
            }
          case (xs, Nil) => appendAll(xs)
          case (Nil, ys) => appendAll(ys)
        }
      loop(lhs.ranges.toList, rhs.ranges.toList)
      new LetterSet(buf.toArray)
    }
}

object LetterSet {

  /**
   * Return an empty set.
   */
  def empty: LetterSet =
    Empty

  /**
   * Return a singleton set containing just `c`.
   */
  def apply(c: Char): LetterSet =
    new LetterSet(Array(c, c))

  /**
   * Return a set containing the given characters.
   */
  def apply(xs: Char*): LetterSet =
    xs.foldLeft(Empty)(_ + _)

  /**
   * Return a set containing the given characters.
   */
  def apply(r: NumericRange[Char]): LetterSet =
    if (r.isInclusive) new LetterSet(Array(r.start, r.end))
    else new LetterSet(Array(r.start, (r.end - 1).toChar))

  /**
   * Return a set containing the given characters.
   */
  def apply(xs: Set[Char]): LetterSet =
    xs.foldLeft(Empty)(_ + _)

  /**
   * Union the given collection of sets into one set.
   */
  def union(css: Iterable[LetterSet]): LetterSet =
    css.foldLeft(LetterSet.empty)(_ | _)

  val Empty: LetterSet =
    new LetterSet(Array.empty[Char])

  val Full: LetterSet =
    new LetterSet(Array(Char.MinValue, Char.MaxValue))

  def escape(c: Char): String =
    Chars.escape(c, Chars.RangeSpecial)

  // assumes lhs and rhs are disjoint

  /**
   * Compute a venn diagram of differences between two logical letter sets.
   *
   * Each list of letter sets is assumed to be disjoint (e.g. no two members of
   * `lhs` should intersect with each other). Every character (or range) that
   * occurs in either of the given lists will be contained in one of the
   * elements of the result.
   *
   * The elements in the result are interpreted as follows:
   *
   *   * Diff.Left(cs): elements in `cs` only occur in `lhs`.
   *   * Diff.Right(cs): elements in `cs` only occur in `rhs`.
   *   * Diff.Both(cs): elements in `cs` occur in `lhs` and `rhs`.
   *
   * Like the input lists, the result list's elements are guaranteed to be
   * disjoint with each other.
   *
   * Each input list is logically equivalent to the union of its elements.
   * However, for efficiency reasons a list is accepted, which allows a more
   * efficient implement of `Rx#firstSet`.
   */
  def venn(lhs: List[LetterSet], rhs: List[LetterSet]): List[Diff[LetterSet]] = {
    type S = LetterSet
    def recur(lhs: List[S], rhs: List[S], res: List[Diff[S]]): List[Diff[S]] =
      lhs match {
        case x0 :: xs =>
          var x = x0
          var r = res
          val b = List.newBuilder[S]
          val it = rhs.iterator
          while (it.hasNext) {
            val y = it.next
            val xy = x & y
            if (xy.isEmpty) {
              b += y
            } else {
              x = x -- xy
              r = Diff.Both(xy) :: r
              val yxy = y -- xy
              if (yxy.nonEmpty) b += yxy
            }
          }
          if (x.nonEmpty) r = Diff.Left(x) :: r
          recur(xs, b.result, r)

        case Nil =>
          rhs.map(Diff.Right(_)) ::: res
      }
    recur(lhs, rhs, Nil)
  }

  /**
   * Iterate over a list of differences between `x` and `y`.
   *
   * This is similar to the venn method, but at a lower level. It operates on
   * individual character ranges rather than entire sets (`Diff[(Char, Char)]`
   * instead of ``Diff[LetterSet]`), and as an `Iterator` rather than a `List`.
   */
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
            val last = Integer.min(x2.toInt, y2.toInt).toChar
            lhs =
              if (x2 > last) ((last + 1).toChar, x2)
              else if (it1.hasNext) it1.next
              else null
            rhs =
              if (y2 > last) ((last + 1).toChar, y2)
              else if (it2.hasNext) it2.next
              else null
            Diff.Both((x1, last))
          }
        }
    }

  def rangeRepr(pair: (Char, Char)): String =
    pair match {
      case (x, y) if x == y => escape(x)
      case (x, y) => s"${escape(x)}-${escape(y)}"
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
