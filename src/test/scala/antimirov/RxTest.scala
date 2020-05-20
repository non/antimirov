package antimirov

import org.scalacheck.{Gen, Prop, Properties, Test}
import org.scalacheck.rng.Seed
import org.typelevel.claimant.Claim

import Prop.{forAllNoShrink => forAll}
//import Rx.{Phi, Empty}

object RxTest extends Properties("RxTest") { self =>

  implicit class PropEq(val lhs: Rx) extends AnyVal {
    def =?=(rhs: Rx): Prop =
      Prop(lhs === rhs) :| s"$lhs === $rhs"
  }

  import java.util.concurrent.{Callable, ForkJoinPool, FutureTask, TimeUnit}

  val pool = new ForkJoinPool()

  def timed(label: => String, warnMs: Long, errorMs: Long)(body: => Prop): Prop = {

    val task = new FutureTask(new Callable[Prop] { def call(): Prop = body })
    pool.execute(task)

    val start = System.nanoTime()
    val warnT = start + warnMs * 1000000L
    val errorT = start + errorMs * 1000000L

    var result: Option[Prop] = None
    var slow = false
    var t0 = start
    while (!task.isDone && t0 < errorT) {
      Thread.sleep(0L, 100000)
      val t1 = System.nanoTime()
      t0 = t1
    }

    if (task.isDone) {
      result = Some(task.get)
    } else {
      task.cancel(true)
    }

    val now = System.nanoTime()
    val duration = (now - start).toDouble / 1000000.0

    if (result.isEmpty) {
      println(s"FAIL {$label} failed to finish within ${errorMs}ms")
    } else if (now >= warnT) {
      println("WARN {%s} took %.1fms (longer than %dms)".format(label, duration, warnMs))
    }

    result.getOrElse(false)
  }

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params
      .withMinSuccessfulTests(200)
      //.withPropFilter(Some("regression"))

  def stringFromRx(r: Rx): Option[Gen[String]] = {
    def recur(r: Rx, s0: Seed, sb0: StringBuilder): (Seed, StringBuilder) =
      r match {
        case Rx.Var(_) => sys.error("!")
        case Rx.Phi => sys.error("!")
        case Rx.Empty => (s0, sb0)
        case Rx.Letter(c) => (s0, sb0 += c)
        case Rx.Letters(cs) =>
          val (n, s1) = s0.long
          val i = ((n & Long.MaxValue) % cs.size).toInt
          val c = cs.iterator.drop(i).next
          (s1, sb0 += c)
        case Rx.Cat(x, y) =>
          val (s1, sb1) = recur(x, s0, sb0)
          recur(y, s1, sb1)
        case Rx.Choice(x, y) =>
          val (n, s1) = s0.long
          if (n < 0L) recur(x, s1, sb0) else recur(y, s1, sb0)
        case s @ Rx.Star(r) =>
          val (n, s1) = s0.long
          if (n < 0L) {
            (s1, sb0)
          } else {
            val (s2, sb1) = recur(r, s1, sb0)
            recur(s, s2, sb1)
          }
      }
    r match {
      case Rx.Var(_) =>
        sys.error("!")
      case Rx.Phi =>
        None
      case _ =>
        Some(Gen.choose(Long.MinValue, Long.MaxValue).map { n =>
          val (_, sb) = recur(r, Seed(n), new StringBuilder)
          sb.toString
        })
    }
  }

  //val (cmin, cmax) = ('a', 'e')
  //val (cmin, cmax) = ('a', 'z')
  //val (cmin, cmax) = (' ', '~')
  val (cmin, cmax) = (Char.MinValue, Char.MaxValue)

  val alphabet: LetterSet =
    LetterSet(cmin to cmax)

  val U: Rx =
    Rx.universe(alphabet)

  val genSym: Gen[Char] =
    Gen.choose(cmin, cmax)

  val genSyms: Gen[LetterSet] =
    Gen.choose(1, 3).flatMap(n => Gen.listOfN(n, genSym)).map(cs => LetterSet(cs.toSet))

  val genInput: Gen[String] =
    Gen.listOf(genSym).map(_.mkString)

  val genEmpty: Gen[String] =
    Gen.const("")

  val genLiteral: Gen[Rx] =
    Gen.oneOf(genSym.map(Rx(_)), genSyms.map(Rx(_)))

  val genBase: Gen[Rx] =
    Gen.lzy(Gen.frequency(
      2 -> genLiteral,
      1 -> (for { x <- genSym; y <- genBase } yield x *: y)))

  def genRecur(depth: Int, f: Int => Gen[Rx]): Gen[Rx] =
    if (depth <= 0) genBase else {
      lazy val g = f(depth - 1)
      Gen.frequency(
        1 -> Gen.const(Rx.Phi),
        //3 -> Gen.const(U),
        10 -> Gen.const(Rx.Empty),
        30 -> genLiteral,
        60 -> (for { x <- genSym; y <- g } yield x *: y),
        20 -> (for { x <- g; y <- g } yield x + y),
        10 -> (for { x <- g } yield x.star))
    }

  lazy val f: Int => Gen[Rx] =
    (n: Int) => genRecur(n, f)

  val genSmallRx: Gen[Rx] = f(3)
  val genRx: Gen[Rx] = f(4)

  val genRxAndStr: Gen[(Rx, Set[String])] =
    genRx.flatMap { r =>
      stringFromRx(r) match {
        case None => Gen.const((r, Set.empty))
        case Some(g) => Gen.listOf(g).map(xs => (r, xs.toSet))
      }
    }

  val scale = 10
  val (warnMs, errorMs) = (50L * scale, 100L * scale)

  // def timedProp[A](name: String, ga: Gen[A])(f: A => Prop): Unit =
  //   self.property(name) = forAll(ga)(f)
  // def timedProp[A, B](name: String, ga: Gen[A], gb: Gen[B])(f: (A, B) => Prop): Unit =
  //   self.property(name) = forAll(ga, gb)(f)
  // def timedProp[A, B, C](name: String, ga: Gen[A], gb: Gen[B], gc: Gen[C])(f: (A, B, C) => Prop): Unit =
  //   self.property(name) = forAll(ga, gb, gc)(f)

  def timedProp[A](name: String, ga: Gen[A])(f: A => Prop): Unit =
    self.property(name) =
      forAll(ga)(a => timed(s"$name : $a", warnMs, errorMs)(f(a)))
  def timedProp[A, B](name: String, ga: Gen[A], gb: Gen[B])(f: (A, B) => Prop): Unit =
    self.property(name) =
      forAll(ga, gb)((a, b) => timed(s"$name : $a : $b", warnMs, errorMs)(f(a, b)))
  def timedProp[A, B, C](name: String, ga: Gen[A], gb: Gen[B], gc: Gen[C])(f: (A, B, C) => Prop): Unit =
    self.property(name) =
      forAll(ga, gb, gc)((a, b, c) => timed(s"$name : $a : $b : $c", warnMs, errorMs)(f(a, b, c)))

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
      case Some(s) => Prop(false) :| s"$rr erroneously matched ${LetterSet.escape(s)}"
    }
  }

  val genIntersect: Gen[(Rx, Rx, Rx, Set[String])] =
    genRx.flatMap { r1 =>
      genRx.flatMap { r2 =>
        val r3 = (r1 & r2)
        stringFromRx(r3) match {
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

//[info] ! RxTest.(x | y)* >= (x* | y*): Falsified after 7 passed tests.
//[info] > ARG_0: /\ud569|\u3244[\u7caa\u9d0a\ud0cc]/
//[info] > ARG_1: /([\u0000-\uffff]*|\u0ee5\uc174[\u415a\ubfdc])*/
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
}
