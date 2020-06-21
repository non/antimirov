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
    antimirov.gen.rx(rx)

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
}
