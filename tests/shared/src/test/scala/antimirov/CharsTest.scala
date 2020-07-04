package antimirov

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.typelevel.claimant.Claim

import Arbitrary.arbitrary
import Prop.{forAllNoShrink => forAll}

object CharsTest extends Properties("CharsTest") {

  def roundTrip(g: Gen[String]): Prop =
    forAll(g) { (s: String) =>
      val t = Chars.escapeStr(s)
      val o = Chars.unescape(t)
      Prop(o == Some(s)) :| s"$o != Some($s) [via $t]"
    }

  property("arbitrary roundtrip") =
    roundTrip(arbitrary[String])

  property("bogus unescapes") = {
    Claim(Chars.unescape("") == None) &&
    Claim(Chars.unescape("\"") == None) &&
    Claim(Chars.unescape("\"x") == None) &&
    Claim(Chars.unescape("x\"") == None) &&
    Claim(Chars.unescape("\"\\q\"") == None) &&
    Claim(Chars.unescape("\"\\u999z\"") == None) &&
    Claim(Chars.unescape("\"\\u9\"") == None)
  }

  property("special roundtrip") = {
    val gc = Gen.oneOf(
      '{', '}', '$', '"', '\n', '\t', '\u0912',
      'a', 'b', 'c', 'd', '~', ' ', '\u0000')
    roundTrip(Gen.listOf(gc).map(_.mkString))
  }
}
