package antimirov

import java.util.regex.Pattern
import org.scalacheck.{Prop, Properties, Test}
import org.typelevel.claimant.Claim
import scala.util.{Failure, Success, Try}

import Parser.RxInterpolation
import Prop.{forAllNoShrink => forAll}
import Util._

object ParserTest extends Properties("ParserTest") { self =>

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params
      .withMinSuccessfulTests(100)
      //.withPropFilter(Some("regression"))

  property("parsing preserves semantics") =
    forAll(genRx) { rx0 =>
      val rx1 = Parser.parse(rx0.reRepr)
      Claim(rx0 === rx1)
    }

  property("parsing handles errors correctly") =
    forAll { (s: String) =>
      Try(Pattern.compile(s)) match {
        case Failure(_) => Claim(Try(Rx.parse(s)).isFailure)
        case Success(_) => Prop(true)
      }
    }

  val interpolations: List[(Rx, Rx)] =
    (rx"[a-c]{2}", Rx('a' to 'c').pow(2)) ::
    (rx"...*", Rx.dot * Rx.dot.plus) ::
    Nil

  property("interpolations") =
    interpolations.map { case (rx0, rx1) =>
      Prop(rx0 == rx1) :| s"rx interpolation produced $rx0 (expected: $rx1)"
    }.reduceLeft(_ && _)

  val validCases: List[(String, Rx)] =
    (".*", Rx.dot.star) ::
    ("\\n+", Rx('\n').plus) ::
    ("[abc]?", Rx(Set('a', 'b', 'c')).optional) ::
    ("[a-z]{3}", Rx('a' to 'z').pow(3)) ::
    ("[^a-z]{1,3}", Rx(~LetterSet('a' to 'z')).repeat(1, 3)) ::
    ("\\u0000|\\\\", Rx(Set('\u0000', '\\'))) ::
    ("[\\u0000abc]", Rx(Set('\u0000', 'a', 'b', 'c'))) ::
    ("∅", Rx.Phi) ::
    Nil

  property("valid cases") =
    validCases.map { case (s, rx1) =>
      val rx0 = Rx.parse(s)
      Prop(rx0 == rx1) :| s"parsed $s got $rx0 (expected: $rx1)"
    }.reduceLeft(_ && _)

  val invalidCases: List[String] =
    "[abc" ::
    "[^abc" ::
    "[]" ::
    "[abc-]" ::
    "a{1, 2}" ::
    "a{1," ::
    "a}" ::
    "a{" ::
    "(abc" ::
    ")abc" ::
    "abc)" ::
    "((abc)*" ::
    "[^^a]" ::
    "\\uwxyz" ::
    "\\u999" ::
    "\\q" ::
    "\\" ::
    "[\\]" ::
    "[\\q]" ::
    "[\\u999]" ::
    "x]" ::
    "x)" ::
    Nil

  property("invalid cases") =
    invalidCases.map { s =>
      try {
        val r = Rx.parse(s)
        Prop(false) :| s"parsed $s got $r (expected failure)"
      } catch {
        case Parser.ParseError(_, _) => Prop(true)
        case t: Throwable => Prop(false) :| s"unexpected error on $s (got $t)"
      }
    }.reduceLeft(_ && _)

  property("regression") = {
    val s = "x]"
    try {
      val r = Rx.parse(s)
      Prop(false) :| s"parsed $s got $r (expected failure)"
    } catch {
      case Parser.ParseError(_, _) => Prop(true)
      case t: Throwable => Prop(false) :| s"unexpected error on $s (got $t)"
    }
  }

}
