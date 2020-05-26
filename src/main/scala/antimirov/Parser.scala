package antimirov

/*

BNF Grammar of Regular Expressions

re        := union | simple-re
union     := re "|" simple-re
simple-re := concat | basic-re
concat    := simple-re basic-re
basic-re  := star | plus | atomic-re
star      := basic-re "*"
plus      := basic-re "+"
repeat    := basic-re "{" int "," int "}" | basic-re "{" int "}"
atomic-re := group | "." | char | set
group     := "(" re ")"
char      := non-metacharacter | "\" escaped
escaped   := unicode | coded | metacharacter
unicode   := "u" hexchar hexchar hexchar hexchar
coded     := "n" | "t" | ...
set       := oneof | noneof
oneof     := "[" items "]"
noneof    := "[^" items "]"
items     := item | item items
item      := range | char
range     := char "-" char

hexchars are 0123456789abcdefABCDEF

metacharacters are {}[]()^$.|*+?\\
non-metacharacters are everything not in metacharacters

 */

object Parser {

  implicit class RxInterpolation(val sc: StringContext) {
    def rx(args: Any*): Rx = Parser.parse(sc.parts.head)
  }

  val Repeat1 = """\{(0|[1-9][0-9]*)\}.*""".r
  val Repeat2 = """\{(0|[1-9][0-9]*),(0|[1-9][0-9]*)\}.*""".r

  def fmap[A, B](pair: (A, Int))(f: A => B): (B, Int) =
    (f(pair._1), pair._2)

  def parse(s: String): Rx = {

    def peek(i: Int): Option[Char] =
      if (i >= s.length) None else Some(s.charAt(i))

    def peekn(i: Int, n: Int): Option[String] =
      if (i + n > s.length) None else Some(s.substring(i, i + n))

    def check(i: Int, c: Char): Boolean =
      peek(i) == Some(c)

    def checkOrDie(i: Int, c: Char): Unit =
      peek(i) match {
        case Some(cc) if c == cc =>
          ()
        case o =>
          val x = o.map(_.toString).getOrElse("eof")
          sys.error(s"at position $i, expected '$c' but got '$x'")
      }

    def parseRe(i: Int): (Rx, Int) = {
      val (rx0, j) = parseSimple(i)
      if (check(j, '|')) fmap(parseRe(j + 1))(rx0 + _) else (rx0, j)
    }

    def parseSimple(i: Int): (Rx, Int) = {
      val (rx0, j) = parseBasic(i)
      peek(j) match {
        case Some('|') | Some(')') | None =>
          (rx0, j)
        case Some(c) =>
          fmap(parseSimple(j))(rx0 * _)
      }
    }

    def parseBasic(i: Int): (Rx, Int) = {
      def recur(rx: Rx, j: Int): (Rx, Int) =
        peek(j) match {
          case Some('+') => recur(rx * rx.star, j + 1)
          case Some('*') => recur(rx.star, j + 1)
          case Some('?') => recur(rx + Rx.empty, j + 1)
          case Some('{') =>
            val (rx1, k) = fmap(parseRepeat(j)) { case (m, n) => rx.repeat(m, n) }
            recur(rx1, k)
          case _ => (rx, j)
        }
      val (rx, j) = parseAtomic(i)
      recur(rx, j)
    }

    def parseRepeat(i: Int): ((Int, Int), Int) =
      s.substring(i) match {
        case Repeat1(sm) =>
          val j = i + 2 + sm.length
          ((sm.toInt, sm.toInt), j)
        case Repeat2(sm, sn) =>
          val j = i + 3 + sm.length + sn.length
          ((sm.toInt, sn.toInt), j)
        case _ => sys.error("!")
      }

    def parseAtomic(i: Int): (Rx, Int) =
      peek(i) match {
        case Some('(') => parseGroup(i + 1)
        case Some('.') => (Rx.dot, i + 1)
        case Some('∅') => (Rx.phi, i + 1)
        case Some('[') => parseSet(i + 1)
        case Some('\\') => fmap(parseEscaped(i + 1, Chars.Special))(Rx.Letter(_))
        case Some(c) if !Chars.Special(c) => (Rx.Letter(c), i + 1)
        case _ => (Rx.empty, i)
      }

    def parseEscaped(i: Int, special: Set[Char]): (Char, Int) =
      peek(i) match {
        case Some('u') =>
          peekn(i + 1, 4) match {
            case Some(Chars.HexChars(t)) =>
              (Integer.parseInt(t, 16).toChar, i + 5)
            case Some(t) =>
              sys.error(s"at position $i, expected 4 hex digits, got '$t'")
            case None =>
              val t = s.substring(i)
              sys.error(s"at position $i, expected 4 hex digits, got '$t'")
          }
        case Some(c) if special(c) => (c, i + 1)
        case Some(c) if Chars.Decoded.contains(c) => (Chars.Decoded(c), i + 1)
        case Some(c) => sys.error(s"at position $i, got invalid escape sequence: '\\$c'")
        case None => sys.error(s"at position $i, expected character, got 'eof'")
      }

    def parseChar(i: Int): (Char, Int) =
      peek(i) match {
        case None =>
          sys.error(s"at position $i, expected character, got 'eof'")
        case Some('\\') =>
          parseEscaped(i + 1, Chars.RangeSpecial)
        case Some(c) if Chars.RangeSpecial(c) =>
          sys.error(s"at position $i, got illegal character '$c'")
        case Some(c) =>
          (c, i + 1)
      }

    def parseItem(i: Int): (LetterSet, Int) = {
      val (c0, j) = parseChar(i)
      if (check(j, '-')) fmap(parseChar(j + 1))(c1 => LetterSet(c0 to c1))
      else (LetterSet(c0 to c0), j)
    }

    def parseItems(i: Int): (LetterSet, Int) = {
      val (ls0, j) = parseItem(i)
      if (check(j, ']')) (ls0, j) else fmap(parseItems(j))(ls0 | _)
    }

    def parseSet(i: Int): (Rx, Int) = {
      val rev = check(i, '^')
      val j = if (rev) i + 1 else i
      val (ls0, k) = parseItems(j)
      val ls = if (rev) ~ls0 else ls0
      checkOrDie(k, ']')
      (Rx.Letters(ls), k + 1)
    }

    def parseGroup(i: Int): (Rx, Int) = {
      val (rx, j) = parseRe(i)
      checkOrDie(j, ')')
      (rx, j + 1)
    }

    val (rx, i) = parseRe(0)
    peek(i) match {
      case None => ()
      case Some(c) => sys.error(s"at position $i, expected 'eof' but got '$c'")
    }
    rx
  }
}
