package antimirov

import org.scalacheck.{Gen, Prop, Properties}
import org.scalacheck.rng.Seed

import Prop.{forAllNoShrink => forAll}

object Util {

  implicit class PropEq(val lhs: Rx) extends AnyVal {
    def =?=(rhs: Rx): Prop =
      Prop(lhs === rhs) :| s"$lhs === $rhs"
  }

  import java.util.concurrent.{Callable, ForkJoinPool, FutureTask}

  val pool = new ForkJoinPool()

  def timed(label: => String, warnMs: Long, errorMs: Long)(body: => Prop): Prop = {

    val task = new FutureTask(new Callable[Prop] { def call(): Prop = body })
    pool.execute(task)

    val start = System.nanoTime()
    val warnT = start + warnMs * 1000000L
    val errorT = start + errorMs * 1000000L

    var result: Option[Prop] = None
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

  def stringsFromRx(r: Rx): Option[Gen[String]] = {
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
  val (cmin, cmax) = (' ', '~')
  //val (cmin, cmax) = (Char.MinValue, Char.MaxValue)

  val alphabet: LetterSet =
    LetterSet(cmin to cmax)

  val U: Rx =
    Rx.closure(alphabet)

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
      stringsFromRx(r) match {
        case None => Gen.const((r, Set.empty))
        case Some(g) => Gen.listOf(g).map(xs => (r, xs.toSet))
      }
    }
}

trait TimingProperties { self: Properties =>

  import Util.timed

  //val scale = 10
  def scale: Long = 10

  def warnMs: Long = 50L * scale
  def errorMs: Long = 100L * scale

  //val (warnMs, errorMs) = (50L * scale, 100L * scale)

  // def timedProp[A](name: String, ga: Gen[A])(f: A => Prop): Unit =
  //   self.property(name) = forAll(ga)(f)
  // def timedProp[A, B](name: String, ga: Gen[A], gb: Gen[B])(f: (A, B) => Prop): Unit =
  //   self.property(name) = forAll(ga, gb)(f)
  // def timedProp[A, B, C](name: String, ga: Gen[A], gb: Gen[B], gc: Gen[C])(f: (A, B, C) => Prop): Unit =
  //   self.property(name) = forAll(ga, gb, gc)(f)

  def timedProp[A](name: String, ga: Gen[A])(f: A => Prop): Unit = {
    self.property(name) =
      forAll(ga)(a => timed(s"$name : $a", warnMs, errorMs)(f(a)))
    ()
  }
  def timedProp[A, B](name: String, ga: Gen[A], gb: Gen[B])(f: (A, B) => Prop): Unit = {
    self.property(name) =
      forAll(ga, gb)((a, b) => timed(s"$name : $a : $b", warnMs, errorMs)(f(a, b)))
    ()
  }
  def timedProp[A, B, C](name: String, ga: Gen[A], gb: Gen[B], gc: Gen[C])(f: (A, B, C) => Prop): Unit = {
    self.property(name) =
      forAll(ga, gb, gc)((a, b, c) => timed(s"$name : $a : $b : $c", warnMs, errorMs)(f(a, b, c)))
    ()
  }
}
