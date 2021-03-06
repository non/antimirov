package antimirov.check

import antimirov.Rx
import org.scalacheck.{Arbitrary, Gen}

/**
 * Regular expression for use with ScalaCheck.
 */
final class Regex(val rx: Rx) { lhs =>

  def accepts(s: String): Boolean =
    rx.accepts(s)

  def rejects(s: String): Boolean =
    rx.rejects(s)

  val gen: Gen[String] =
    Regex.gen(rx)

  override def toString: String =
    s"$rx"

  override def hashCode: Int =
    rx.hashCode

  override def equals(that: Any): Boolean =
    that match {
      case r: Regex => rx == r.rx
      case _ => false
    }

  case class Word private (value: String) {
    override def toString: String = value
  }

  object Word {
    implicit val arbitraryForWord: Arbitrary[Word] =
      Arbitrary(gen.map(Word(_)))
  }
}

object Regex {

  def apply(s: String): Regex =
    new Regex(Rx.parse(s))

  def apply(r: Rx): Regex =
    new Regex(r)

  /**
   * Produce a Gen[String] for the given regular expression.
   *
   * In theory, any string accepted by the regular expression can be
   * generated. However, due to the statistical distribution used to
   * expand the Kleene star operators, extremely long strings have a
   * vanishingly small chance of being generated.
   */
  def gen(r: Rx): Gen[String] =
    r match {
      // $COVERAGE-OFF$
      case v @ Rx.Var(_) =>
        sys.error(s"illegal regex detected ($v)")
      // $COVERAGE-ON$
      case Rx.Phi =>
        sys.error(s"impossible to generate strings from regex ϕ")
      case Rx.Empty =>
        Gen.const("")
      case Rx.Letter(c) =>
        Gen.const(c.toString)
      case Rx.Letters(cs) =>
        Gen.choose(0, cs.size - 1).map(cs.get(_).toString)
      case c @ Rx.Concat(_, _) =>
        val gs = Rx.parseConcats(c.concats).map {
          case Left(s) => Gen.const(s)
          case Right(r) => gen(r)
        }
        Gen.sequence(gs).map { lst =>
          // it'd be nice to use java converters but bridging 2.12 and
          // 2.13 is annoying due to deprecation. this works fine.
          val sb = new StringBuilder
          val it = lst.iterator
          while (it.hasNext) sb.append(it.next)
          sb.toString
        }
      case c @ Rx.Choice(_, _) =>
        // TODO: would be nice to weight choices by how many options there are
        val gs = c.choices.map(gen(_))
        Gen.oneOf(gs).flatMap(identity)
      case Rx.Star(r) =>
        val gr = gen(r)
        // on average we'll expand a Kleene star 3 times. (most of the
        // time we'll do fewer than 3 expansions, but the occasional
        // large outliers pull the mean up to 3.)
        geometric(3.0)
          .flatMap(sz => Gen.listOfN(sz, gr))
          .map(_.mkString)
      case Rx.Repeat(r, m, n) =>
        val gr = gen(r)
        val mod = n - m + 1
        // we use (_ % mod) to wrap larger values from the geometric
        // distribution around. while this lowers the mean a bit, it
        // ensures we end up in the correct [m, n] range.
        geometric(3.0)
          .flatMap(sz => Gen.listOfN(m + (sz % mod), gr))
          .map(_.mkString)
    }

  // generate an integer from the geometric distribution with the
  // given mean. the range of values generated are always [0, ∞).
  private def geometric(mean: Double): Gen[Int] = {
    require(mean > 0.0, s"mean must be positive (got: $mean)")
    val p = 1.0 / (mean + 1.0)
    val lognp = Math.log1p(-p) // log(1 - p)
    Gen.choose(0.0, 1.0).map { u =>
      Math.floor(Math.log(u) / lognp).toInt
    }
  }

}
