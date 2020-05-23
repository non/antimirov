package antimirov

import java.util.regex.Pattern
import org.scalacheck.{Gen, Prop, Properties, Test}
import org.scalacheck.rng.Seed
import org.typelevel.claimant.Claim

import Prop.{forAllNoShrink => forAll}
import Util._

object RxTest extends Properties("RxTest") with TimingProperties { self =>

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params
      .withMinSuccessfulTests(100)
      //.withPropFilter(Some("regression"))

  //override def scale: Long = 10L

  timedProp("regex accepts string", genRxAndStr) { case (r, lst) =>
    lst.forall(r.accepts)
  }

  timedProp("regex union accepts", genRxAndStr, genRxAndStr) {
    case ((r1, lst1), (r2, lst2)) =>
      val r = r1 + r2
      (lst1 | lst2).forall(r.accepts)
  }

  timedProp("regex not accepts", genRxAndStr) { case (r, lst) =>
    val rr = U - r
    lst.find(s => rr.accepts(s)) match {
      case None => Prop(true)
      case Some(s) => Prop(false) :| s"$rr erroneously matched ${Chars.escape(s)}"
    }
  }

  val genIntersect: Gen[(Rx, Rx, Rx, Set[String])] =
    genRx.flatMap { r1 =>
      genRx.flatMap { r2 =>
        val r3 = (r1 & r2)
        stringsFromRx(r3) match {
          case None => Gen.const((r1, r2, r3, Set.empty))
          case Some(g) => Gen.listOf(g).map(xs => (r1, r2, r3, xs.toSet))
        }
      }
    }

  timedProp("regex intersection accepts", genIntersect) { case (r1, r2, r3, lst) =>
    lst.find(s => r1.rejects(s) || r2.rejects(s)) match {
      case None =>
        Prop(true)
      case Some(s) =>
        val (b1, b2) = (r1.rejects(s), r2.rejects(s))
        Prop(false) :| s"($r1.accepts($s) = $b1) & ($r2.accepts($s) = $b2)"
    }
  }

  timedProp("x = x", genRx) { x =>
    x =?= x
  }

  timedProp("x & y = y & x", genRx, genRx) { (x, y) =>
    (x & y) =?= (y & x)
  }

  timedProp("(x & x) = x", genRx) { x =>
    (x & x) =?= x
  }

  timedProp("((x & y) & y) = (x & y)", genRx, genRx) { (x, y) =>
    val xy = x & y
    (xy & y) =?= xy
  }

  timedProp("(x | x) = x", genRx) { x =>
    (x | x) =?= x
  }

  timedProp("((x | y) | y) = (x | y)", genRx, genRx) { (x, y) =>
    val xy = x | y
    (xy | y) =?= xy
  }

  timedProp("x | y = y | x", genRx, genRx) { (x, y) =>
    (x + y) =?= (y + x)
  }

  timedProp("(x - y) + y >= x", genRx, genRx) { (x, y) =>
    val xyy = ((x - y) + y)
    Claim(xyy >= x) :| s"$xyy >= $x"
  }

  property("regression") = {
    val x = Rx('\ud569') + (Rx('\u3244') * (Rx('\u7caa') + Rx('\u9d0a') + Rx('\ud0cc')))
    val y = (Rx(LetterSet.Full).star + Rx("\u0ee5\uc174") * (Rx('\u415a') + Rx('\ubfdc'))).star
    val lhs = (x + y).star
    val rhs = x.star + y.star
    Prop(lhs >= rhs)
  }

  timedProp("U - (U - x) = x", genRx) { x =>
    (U - (U - x)) =?= x
  }

  timedProp("(x ^ y) = (x | y) - (x & y)", genRx, genRx) { (x, y) =>
    (x ^ y) =?= ((x | y) - (x & y))
  }

  timedProp("x** = x*", genRx) {
    case s @ Rx.Star(x) => s.star =?= s
    case x => x.star.star =?= x.star
  }

  timedProp("xx*|ε = x*", genRx) { x =>
    (Rx.Empty + (x * x.star)) =?= x.star
  }

  timedProp("(x | y)* >= (x* | y*)", genSmallRx, genSmallRx) { (x, y) =>
    val lhs = ((x + y).star)
    val rhs = (x.star + y.star)
    Claim(lhs supersetOf rhs) :| s"$lhs >= $rhs"
  }

  timedProp("x∅ = ∅x = ∅", genRx) { x =>
    val lhs = (x * Rx.Phi)
    val rhs = (Rx.Phi * x)
    Claim((lhs == Rx.Phi) && (rhs == Rx.Phi)) :| s"$lhs = $rhs = ∅"
  }

  timedProp("x * (y + z) = (x * y) + (x * z)", genRx, genRx, genRx) { (x, y, z) =>
    (x * (y + z)) =?= ((x * y) + (x * z))
  }

  timedProp("(x + y) * z = (x * z) + (y * z)", genRx, genRx, genRx) { (x, y, z) =>
    ((x + y) * z) =?= ((x * z) + (y * z))
  }

  timedProp("(x = y) = ((x partialCompare y) == 0)", genRx, genRx) { (x, y) =>
    val lhs = x === y
    val c = x partialCompare y
    val rhs = c == 0.0
    Claim(lhs == rhs) :| s"($x = $y) = ($c = 0)"
  }

  timedProp("(x = y) = ((x & y) = x = y)", genRx, genRx) { (x, y) =>
    val lhs = x === y
    val xy = x & y
    val rhs = (xy === x) && (xy === y)
    Claim(lhs == rhs) :| s"($x = $y) = (($xy = $x) & ($xy = $y))"
  }

  timedProp("(x <= y) = ((x & y) = x)", genRx, genRx) { (x, y) =>
    val lhs = x subsetOf y
    val rhs = ((x & y) === x)
    Claim(lhs == rhs) :| s"($x <= $y) = (${x & y} = $x)"
  }

  timedProp("x <= y -> y accepts x", genRxAndStr, genRxAndStr) { case ((x, lstx), (y, lsty)) =>
    val n = x partialCompare y
    if (n == 0.0) {
      (lstx | lsty).forall(s => x.accepts(s) && y.accepts(s))
    } else if (n < 0.0) {
      lstx.forall(y.accepts)
    } else if (n > 0.0) {
      lsty.forall(x.accepts)
    } else { /* NaN */
      true
    }
  }

  def comparePatterns(r: Rx, p: Pattern, lst: Set[String]): Prop =
    lst.map { s =>
      val lhs = r.accepts(s)
      val rhs = p.matcher(s).matches
      Prop(lhs == rhs) :| s"disagreement for '$s' :: ${r.repr} -> $lhs != $rhs"
    }.foldLeft(Prop(true))(_ && _)

  timedProp("matches java", genRxAndStr, genRxAndStr) { case ((x, lstx), (y, lsty)) =>
    val (px, py) = (x.toJava, y.toJava)
    val lst = lstx | lsty
    comparePatterns(x, px, lst) && comparePatterns(y, py, lst)
  }

  timedProp("matches scala", genRxAndStr, genRxAndStr) { case ((x, lstx), (y, lsty)) =>
    val (px, py) = (x.toScala.pattern, y.toScala.pattern)
    val lst = lstx | lsty
    comparePatterns(x, px, lst) && comparePatterns(y, py, lst)
  }
}
