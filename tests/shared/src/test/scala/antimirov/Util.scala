package antimirov

import antimirov.check.Regex
import org.scalacheck.{Arbitrary, Gen, Prop}

import Arbitrary.arbitrary

object Util {

  implicit class PropEq(val lhs: Rx) extends AnyVal {
    def =?=(rhs: Rx): Prop =
      Prop(lhs === rhs) :| s"/$lhs/ === /$rhs/"
  }

  def timer[A](name: String)(body: => A): A = {
    //val _: A = body // warmup
    val t0 = System.nanoTime()
    val a1: A = body // real
    val t = (System.nanoTime() - t0) / 1000000.0
    val s = a1.toString
    val limit = 20
    if (s.length >= limit) {
      println(s"$name took $t ms (${s.substring(0, limit)}...)")
    } else {
      println(s"$name took $t ms ($s)")
    }
    a1
  }

  def stringsFromRx(r: Rx): Option[Gen[String]] =
    r match {
      case Rx.Phi => None
      case r => Some(Regex.gen(r))
    }

  // using a very restricted set of characters (a-e) makes things like
  // intersection very intensive, since you are likely to have many
  // patterns using the same characters. using a very large set can be
  // challenging for letter set/letter map and large unions, but is
  // less intensive for the rx tests themselves.

  // TODO: we get random coder exceptions when using the widest range
  // on some generated characters. this is due to mismatched surrogate
  // pairs, which our generators aren't smart enough to prevent.

  //val (cmin, cmax) = ('a', 'e')
  val (cmin, cmax) = ('a', 'z')
  //val (cmin, cmax) = (' ', '~')
  //val (cmin, cmax) = ('\u0000', '\ud7ff')
  //val (cmin, cmax) = ('\u0000', '\uffff')

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
      1 -> (for { x <- genSym; y <- genBase } yield Rx(x) * y)))

  def genRecur(depth: Int, f: Int => Gen[Rx]): Gen[Rx] =
    if (depth <= 0) genBase else {
      lazy val g = f(depth - 1)
      Gen.frequency(
        1 -> Gen.const(Rx.Phi),
        3 -> Gen.const(U),
        2 -> (for {
          x <- g
          m <- Gen.choose(0, 1)
          n <- Gen.choose(1, 2)
        } yield x.repeat(m, Integer.max(m, n))),
        10 -> Gen.const(Rx.Empty),
        30 -> genLiteral,
        60 -> (for { x <- genSym; y <- g } yield Rx(x) * y),
        15 -> (for { x <- g; y <- g } yield x + y),
        5 -> (for { x <- g } yield x.star))
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

  val genRxAndStrs: Gen[(Rx, Set[String])] =
    Gen.zip(genRxAndStr, arbitrary[Set[String]])
      .map { case ((rx, good), maybe) => (rx, good | maybe) }
}
