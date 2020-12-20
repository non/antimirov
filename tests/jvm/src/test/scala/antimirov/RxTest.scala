package antimirov

import java.lang.Integer.signum
import java.util.regex.Pattern
import org.scalacheck.{Gen, Prop, Properties, Test}
import org.typelevel.claimant.Claim
import scala.util.{Failure, Success, Try}

import Util._

object RxTest extends Properties("RxTest") with TimingProperties { self =>

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params
      .withMinSuccessfulTests(100)
      //.withPropFilter(Some("regression"))

  override def scale: Long = 20L
  override def enableTiming = true
  override def failOnAbort = false

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
      case Some(s) => Prop(false) :| s"$rr erroneously matched ${Chars.escapeStr(s)}"
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
    val x = Rx.parse("k(sww|((ba)*{1,2}))")
    try {
      x.cardinality >= Size.Zero
    } catch { case e: Throwable =>
      e.printStackTrace()
      false
    }
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

  timedProp("(x < y) = (~x > ~y)", genRx, genRx) { (x, y) =>
    Claim((x properSubsetOf y) == ((~x) properSupersetOf (~y)))
  }

  timedProp("r.reverse accepts reversed strings", genRxAndStr) { case (x, lst) =>
    val r = x.reverse
    lst.forall(s => r.accepts(s.reverse))
  }

  timedProp("r.canonical = r", genRx) { r =>
    Claim(r.canonical === r)
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

  timedProp("x.pow(k) = x * x * ...", genSmallRx, Gen.choose(0, 4)) { (rx, k) =>
    val lhs = rx.pow(k)
    val rhs = (1 to k).map(_ => rx).foldLeft(Rx.empty)(_ * _)
    lhs =?= rhs
  }

  // property("timing") = {
  //   List(2, 4, 8, 16, 32, 64).forall { n =>
  //     val str = "[0-9a-f]" * n
  //     val rx = Rx.parse(str)
  //     val data = "c63621736219939429349a736271367213627362713627361277223743646784593f854"
  //     val short = data.substring(0, n - 1)
  //     val good = data.substring(0, n)
  //     val long = data.substring(0, n + 1)
  //
  //
  //     println(s"n=$n")
  //     timer(s" - hex1($n): r <= U")(rx <= Rx.Universe)
  //     timer(s" - hex2($n): r <= r*")(rx <= rx.star)
  //     timer(s" - hex3($n): r & U")(rx & Rx.Universe)
  //     timer(s" - hex4($n): r accepts good")(rx.accepts(good))
  //     timer(s" - hex5($n): r rejects short")(rx.rejects(short))
  //     timer(s" - hex6($n): r rejects long")(rx.rejects(long))
  //     println("")
  //
  //     true
  //   }
  // }

  def comparePatterns(r: Rx, p: Pattern, lst: Set[String]): Prop =
    lst.map { s =>
      val lhs = r.accepts(s)
      val rhs = p.matcher(s).matches
      Prop(lhs == rhs) :| s"disagreement for '$s' :: $r -> $lhs != $rhs"
    }.foldLeft(Prop(true))(_ && _)

  // we use Try in the next two tests to deal with invalid strings
  // that we randomly generate. antimirov doesn't care which strings
  // are (or aren't) valid UTF-16 but Java does, so we have a chance
  // to produce invalid strings in our generators.

  timedProp("matches java", genRxAndStr, genRxAndStr) { case ((x, lstx), (y, lsty)) =>
    Try((x.toJava, y.toJava)) match {
      case Success((px, py)) =>
        val lst = lstx | lsty
        comparePatterns(x, px, lst) && comparePatterns(y, py, lst)
      case Failure(_) =>
        Prop(true)
    }
  }

  timedProp("matches scala", genRxAndStr, genRxAndStr) { case ((x, lstx), (y, lsty)) =>
    Try((x.toScala.pattern, y.toScala.pattern)) match {
      case Success((px, py)) =>
         val lst = lstx | lsty
        comparePatterns(x, px, lst) && comparePatterns(y, py, lst)
      case Failure(_) =>
        Prop(true)
    }
  }

  timedProp("cardinality matches star-depth", genRx) { r =>
    Claim((r.cardinality == Size.Unbounded) == (r.starDepth > 0))
  }

  timedProp("scalaRepr", genRx) { r =>
    Claim(r.scalaRepr != "") // kind of bogus, just exercise the code
  }

  timedProp("cardRepr", genSmallRx) { r =>
    Claim(r.cardRepr != "") // kind of bogus, just exercise the code
  }

  property("cardinality") = {
    Claim(Rx.parse(".*").cardinality == Size.Unbounded)
  }

  property("Rx(s) = Rx.concat(s.map(Rx(_)))") =
    Prop.forAll { (s: String) =>
      Claim(Rx(s) === Rx.concat(s.map(Rx(_))))
    }

  import Rx.lexCompare

  def lteq(x: Rx, y: Rx): Boolean =
    lexCompare(x, y) <= 0

  property("Ordering[Rx]: lexCompare(x, y) = lexCompare(y, x) * -1") =
    Prop.forAll(genRx, genRx) { (x: Rx, y: Rx) =>
      Claim(lexCompare(x, y) == -lexCompare(y, x))
    }

  property("Ordering[Rx]: (lexCompare(x, y) == 0) = (x === y)") =
    Prop.forAll(genRx, genRx) { (x: Rx, y: Rx) =>
      Claim((lexCompare(x, y) == 0) == (x === y))
    }

  property("Ordering[Rx]: transitivity") =
    Prop.forAll(genRx, genRx, genRx) { (x: Rx, y: Rx, z: Rx) =>
      if (lteq(x, y) && lteq(y, z)) {
        Claim(lteq(x, z))
      } else {
        Claim(true)
      }
    }

  property("Ordering[Rx]: identity") =
    Prop.forAll(genRx) { (x: Rx) =>
      Claim(lexCompare(x, x) == 0)
    }

  property("Ordering[Rx]: Phi <= x") =
    Prop.forAll(genRx) { (x: Rx) =>
      Claim(lteq(Rx.phi, x))
    }

  property("Ordering[Rx]: U >= x") =
    Prop.forAll(genRx) { (x: Rx) =>
      Claim(lteq(x, U))
    }

  property("Ordering[Rx]: (s1 compare s2) = lexCompare(Rx(s1), Rx(s2))") =
    Prop.forAll { (s1: String, s2: String) =>
      Claim(signum(s1 compare s2) == signum(lexCompare(Rx(s1), Rx(s2))))
    }

  property("Ordering[Rx]: rx.accepts(s) ~ lexCompare(rx, Rx(s))") =
    Prop.forAll(genRx, genInput) { (rx: Rx, s: String) =>
      if (rx.accepts(s)) {
        Claim(lexCompare(Rx(s), rx) <= 0)
      } else {
        Claim(lexCompare(Rx(s), rx) != 0)
      }
    }

  property("Ordering[Rx]: lexCompare(x, y) ~ partialCompare(x, y)") =
    Prop.forAll(genRx, genRx) { (x: Rx, y: Rx) =>
      (x partialCompare y) match {
        case 0.0 => Claim(lexCompare(x, y) == 0)
        case n if n > 0.0 => Claim(lexCompare(x, y) > 0)
        case n if n < 0.0 => Claim(lexCompare(x, y) < 0)
        case _ /* nan */ => Claim(lexCompare(x, y) != 0)
      }
    }
}
