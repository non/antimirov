package antimirov

sealed abstract class Size { lhs =>

  import Size.{Finite, Unbounded}

  def +(rhs: Size): Size =
    (lhs, rhs) match {
      case (Finite(x), Finite(y)) => Finite(x + y)
      case _ => Unbounded
    }

  def *(rhs: Size): Size =
    (lhs, rhs) match {
      case (Size.Zero, _) | (_, Size.Zero) => Size.Zero
      case (Finite(x), Finite(y)) => Finite(x * y)
      case _ => Unbounded
    }

  override def toString: String =
    this match {
      case Unbounded => "∞"
      case Finite(n) => n.toString
    }

  def <(rhs: Size): Boolean = compare(rhs) < 0
  def <=(rhs: Size): Boolean = compare(rhs) <= 0
  def >(rhs: Size): Boolean = compare(rhs) > 0
  def >=(rhs: Size): Boolean = compare(rhs) >= 0

  def min(rhs: Size): Size =
    if (lhs <= rhs) lhs else rhs

  def max(rhs: Size): Size =
    if (lhs >= rhs) lhs else rhs

  def compare(rhs: Size): Int =
    (lhs, rhs) match {
      case (Finite(x), Finite(y)) => Integer.compare(x, y)
      case (Finite(_), Unbounded) => -1
      case (Unbounded, Finite(_)) => 1
      case (Unbounded, Unbounded) => 0
    }
}

object Size {

  val Zero = Size(0)
  val One = Size(1)

  def apply(n: Int): Size = {
    require(n >= 0)
    Finite(n)
  }

  case class Finite private[antimirov] (n: Int) extends Size
  case object Unbounded extends Size
}
