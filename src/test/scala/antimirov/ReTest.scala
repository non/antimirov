package antimirov

import org.scalacheck.{Gen, Prop, Properties, Test}
import org.scalacheck.rng.Seed
import org.typelevel.claimant.Claim

import Prop.{forAllNoShrink => forAll}
import Re.{Phi, Empty}

object ReTest extends Properties("ReTest") {

  def timed(warnMs: Long, errorMs: Long, label: => String)(body: => Boolean): Boolean = body

  def timed0(warnMs: Long, errorMs: Long, label: => String)(body: => Boolean): Boolean = {
    import java.util.concurrent.{Callable, FutureTask, TimeUnit}

    val task = new FutureTask(new Callable[Boolean] { def call(): Boolean = body })
    task.run()
    var result: Option[Boolean] = None

    val start = System.nanoTime()
    val warnT = start + warnMs * 1000000L
    val errorT = start + errorMs * 1000000L

    var slow = false
    var t0 = start
    while (!task.isDone && t0 < errorT) {
      Thread.sleep(10L)
      t0 = System.nanoTime()
    }

    if (task.isDone) {
      result = Some(task.get)
    } else {
      task.cancel(true)
    }

    val now = System.nanoTime()

    if (result.isEmpty) {
      println(s"$label: failed to finish within ${errorMs}ms")
    } else if (now >= warnT) {
      println(s"$label: took longer than ${warnMs}ms")
    }

    result.getOrElse(false)
  }

  override def overrideParameters(ps: Test.Parameters): Test.Parameters =
    ps.withMinSuccessfulTests(1000)

  val alphabet: Set[Char] =
    Set('a', 'b', 'c')

  val U: Re =
    Re.universe(alphabet)

  val genSym: Gen[Char] =
    Gen.oneOf(alphabet.toList)

  val genInput: Gen[String] =
    Gen.listOf(genSym).map(_.mkString)

  val genEmpty: Gen[String] =
    Gen.const("")

  val genLiteral: Gen[Re] =
    genSym.map(Re(_))

  def stringFromRe(r: Re): Option[Gen[String]] = {
    def recur(r: Re, s0: Seed, sb0: StringBuilder): (Seed, StringBuilder) =
      r match {
        case Re.Var(_) => sys.error("!")
        case Re.Phi => sys.error("!")
        case Re.Empty => (s0, sb0)
        case Re.Letter(c) => (s0, sb0 += c)
        case Re.Cat(x, y) =>
          val (s1, sb1) = recur(x, s0, sb0)
          recur(y, s1, sb1)
        case Re.Choice(x, y) =>
          val (n, s1) = s0.long
          if (n < 0L) recur(x, s1, sb0) else recur(y, s1, sb0)
        case s @ Re.Star(r) =>
          val (n, s1) = s0.long
          if (n < 0L) {
            (s1, sb0)
          } else {
            val (s2, sb1) = recur(r, s1, sb0)
            recur(s, s2, sb1)
          }
      }
    r match {
      case Re.Var(_) =>
        sys.error("!")
      case Re.Phi =>
        None
      case _ =>
        Some(Gen.choose(Long.MinValue, Long.MaxValue).map { n =>
          val (_, sb) = recur(r, Seed(n), new StringBuilder)
          sb.toString
        })
    }
  }
/*
  def stringFromRe(r: Re): Option[Gen[String]] =
    r match {
      case v @ Re.Var(_) =>
        sys.error(s"unsupported: $v")
      case Phi =>
        None
      case Empty =>
        Some(genEmpty)
      case Re.Letter(c) =>
        Some(Gen.const(c.toString))
      case Re.Choice(x, y) =>
        (stringFromRe(x), stringFromRe(y)) match {
          case (Some(gx), Some(gy)) => Some(Gen.oneOf(gx, gy))
          case (some @ Some(_), None) => some
          case (None, some @ Some(_)) => some
          case (None, None) => None
        }
      case Re.Cat(x, y) =>
        for {
          gx <- stringFromRe(x)
          gy <- stringFromRe(y)
        } yield for {
          sx <- gx
          sy <- gy
        } yield sx ++ sy
      case r @ Re.Star(x) =>
        stringFromRe(x) match {
          case None =>
            Some(genEmpty)
          case Some(gx) =>
            lazy val g: Gen[String] =
              Gen.lzy(Gen.oneOf(
                genEmpty,
                for { sx <- gx; s <- g } yield sx ++ s))
            Some(g)
        }
    }
 */

  val genBase: Gen[Re] =
    Gen.lzy(Gen.frequency(
      2 -> genLiteral,
      1 -> (for { x <- genSym; y <- genBase } yield x *: y)))

  def genRecur(depth: Int, f: Int => Gen[Re]): Gen[Re] =
    if (depth <= 0) genBase else {
      lazy val g = f(depth - 1)
      Gen.frequency(
        1 -> Gen.const(Phi),
        5 -> Gen.const(Empty),
        20 -> genLiteral,
        40 -> (for { x <- genSym; y <- g } yield x *: y),
        10 -> (for { x <- g; y <- g } yield x + y),
        5 -> (for { x <- g } yield x.star))
    }

  lazy val genRe: Gen[Re] = {
    lazy val f: Int => Gen[Re] =
      (n: Int) => genRecur(n, f)
    f(4)
  }

  val genReAndString: Gen[(Re, Set[String])] =
    genRe.flatMap { r =>
      stringFromRe(r) match {
        case None => Gen.const((r, Set.empty))
        case Some(g) => Gen.listOf(g).map(xs => (r, xs.toSet))
      }
    }

  property("regex accepts string") =
    forAll(genReAndString) { case (r, lst) =>
      timed(5L, 10L, s"$r : $lst") {
        lst.forall(r.accepts)
      }
    }

  property("regex union accepts") =
    forAll(genReAndString, genReAndString) { case ((r1, lst1), (r2, lst2)) =>
      timed(5L, 10L, s"$r1 : $lst1 : $r2 : $lst2") {
        val r = r1 + r2
          (lst1 | lst2).forall(r.accepts)
      }
    }

  property("regex not accepts") =
    forAll(genReAndString) { case (r, lst) =>
      timed(5L, 10L, s"$r : $lst") {
        val rr = U - r
        lst.forall(s => !rr.accepts(s))
      }
    }

  val genIntersect: Gen[(Re, Re, Re, Set[String])] =
    genRe.flatMap { x1 =>
      genRe.flatMap { x2 =>
        val (r1, r2) = (x1.simplified, x2.simplified)
        val r3 = (r1 & r2).simplified
        stringFromRe(r3) match {
          case None => Gen.const((r1, r2, r3, Set.empty))
          case Some(g) => Gen.listOf(g).map(xs => (r1, r2, r3, xs.toSet))
        }
      }
    }

  property("regex intersection accepts") =
    forAll(genIntersect) { case (r1, r2, r3, lst) =>
      timed(5L, 10L, s"$r1 : $r2 : $r3 : $lst") {
        lst.forall(s => r1.accepts(s) && r2.accepts(s))
      }
    }

  property("x = x") =
    forAll(genRe) { x =>
      timed(5L, 10L, s"$x") {
        x === x
      }
    }

  property("x & y = y & x") =
    forAll(genRe, genRe) { (x, y) =>
      timed(5L, 10L, s"$x : $y") {
        val lhs = (x & y)
        val rhs = (y & x)
        lhs === rhs
      }
    }

  property("(x & x) = x") =
    forAll(genRe) { x =>
      timed(5L, 10L, s"$x") {
        (x & x) === x
      }
    }

  property("((x & y) & y) = (x & y)") =
    forAll(genRe, genRe) { (x, y) =>
      timed(5L, 10L, s"$x : $y") {
        val xy = x & y
          (xy & y) === xy
      }
    }

  property("(x | x) = x") =
    forAll(genRe) { x =>
      timed(5L, 10L, s"$x") {
        (x | x) === x
      }
    }

  property("((x | y) | y) = (x | y)") =
    forAll(genRe, genRe) { (x, y) =>
      timed(5L, 10L, s"$x : $y") {
        val xy = x | y
        (xy | y) === xy
      }
    }

  property("x | y = y | x") =
    forAll(genRe, genRe) { (x, y) =>
      timed(5L, 10L, s"$x : $y") {
        (x + y) === (y + x)
      }
    }

  property("(x - y) + y >= x") =
    forAll(genRe, genRe) { (x, y) =>
      timed(5L, 10L, s"$x : $y") {
        ((x - y) + y) >= x
      }
    }

  property("U - (U - x) = x") =
    forAll(genRe) { x =>
      timed(5L, 10L, s"$x") {
        (U - (U - x)) === x
      }
    }

  property("(x ^ y) = (x | y) - (x & y)") =
    forAll(genRe, genRe) { (x, y) =>
      timed(5L, 10L, s"$x : $y") {
        (x ^ y) === ((x | y) - (x & y))
      }
    }

  property("x** = x*") =
    forAll(genRe) { x =>
      timed(5L, 10L, s"$x") {
        x.star.star === x.star
      }
    }

  property("xx*|ε = x*") =
    forAll(genRe) { x =>
      timed(5L, 10L, s"$x") {
        (Empty + (x * x.star)) === x.star
      }
    }

  property("(x | y)* >= (x* | y*)") =
    forAll(genRe, genRe) { (x, y) =>
      timed(5L, 10L, s"$x : $y") {
        ((x + y).star) supersetOf (x.star + y.star)
      }
    }

  property("x∅ = ∅x = ∅") =
    forAll(genRe) { x =>
      timed(5L, 10L, s"$x") {
        ((x * Phi) == Phi) && ((Phi * x) == Phi)
      }
    }

  property("x * (y + z) = (x * y) + (x * z)") =
    forAll(genRe, genRe, genRe) { (x, y, z) =>
      timed(5L, 10L, s"$x : $y : $z") {
        (x * (y + z)) === ((x * y) + (x * z))
      }
    }

  property("(x + y) * z = (x * z) + (y * z)") =
    forAll(genRe, genRe, genRe) { (x, y, z) =>
      timed(5L, 10L, s"$x : $y : $z") {
        ((x + y) * z) === ((x * z) + (y * z))
      }
    }

  property("(x = y) = ((x partialCompare y) == 0)") =
    forAll(genRe, genRe) { (x, y) =>
      timed(5L, 10L, s"$x : $y") {
        val lhs = x === y
        val rhs = (x partialCompare y) == 0.0
        lhs == rhs
      }
    }

  property("(x = y) = ((x & y) = x = y)") =
    forAll(genRe, genRe) { (x, y) =>
      timed(5L, 10L, s"$x : $y") {
        val lhs = x === y
        val xy = x & y
        val rhs = (xy === x) && (xy === y)
        lhs == rhs
      }
    }

  property("(x <= y) = ((x & y) = x)") =
    forAll(genRe, genRe) { (x, y) =>
      timed(5L, 10L, s"$x : $y") {
        val lhs = x subsetOf y
        val rhs = ((x & y) === x)
        lhs == rhs
      }
    }

  property("x <= y -> y accepts x") =
    forAll(genReAndString, genReAndString) { case ((x, lstx), (y, lsty)) =>
      timed(5L, 10L, s"$x : $y") {
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
    }
}
