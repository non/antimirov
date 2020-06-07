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

  def isEmpty: Boolean = keys.length == 0

  def nonEmpty: Boolean = keys.length != 0

  def merge(rhs: LetterMap[A])(f: (A, A) => A)(implicit ct: ClassTag[A]): LetterMap[A] = {
    val it = LetterSet.diff(lhs.keySet, rhs.keySet)
    val kbuf = mutable.ArrayBuffer.empty[Char]
    val vbuf = mutable.ArrayBuffer.empty[A]
    while (it.hasNext) {
      val diff = it.next
      val (c1, c2) = diff.value
      kbuf.append(c1)
      kbuf.append(c2)
      vbuf.append(diff match {
        case Diff.Left((c, _)) => lhs(c)
        case Diff.Both((c, _)) => f(lhs(c), rhs(c))
        case Diff.Right((c, _)) => rhs(c)
      })
    }
    new LetterMap(kbuf.toArray, vbuf.toArray)
  }

  def mapValues[B: ClassTag](f: A => B): LetterMap[B] =
    new LetterMap(keys, vals.map(f))

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
