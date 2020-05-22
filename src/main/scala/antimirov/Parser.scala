package antimirov

/*

BNF Grammar of Regular Expressions

<RE>        ::= <union> | <simple-RE>
<union>     ::= <RE> "|" <simple-RE>
<simple-RE> ::= <concat> | <basic-RE>
<concat>    ::= <simple-RE> <basic-RE>
<basic-RE>  ::= <star> | <plus> | <atomic-RE>
<star>      ::= <atomic-RE> "*"
<plus>      ::= <atomic-RE> "+"
<atomic-RE> ::= <group> | <dot> | <char> | <set>
<group>     ::= "(" <RE> ")"
<dot>       ::= "."
<char>      ::= any non metacharacter | "\" metacharacter
<set>       ::= <oneof> | <noneof>
<oneof>     ::= "[" <items> "]"
<noneof>    ::= "[^" <items> "]"
<items>     ::= <item> | <item> <items>
<item>      ::= <range> | <char>
<range>     ::= <char> "-" <char>
 */

object Parser {

  val dot: Rx = Rx.Letters(LetterSet.Full)

  val special: Set[Char] =
    "{}[]()^$.|*+?\\".toSet

  def parse(s: String): Rx = {

    def peek(i: Int): Option[Char] =
      if (i >= s.length) None
      else Some(s.charAt(i))

    def check(i: Int, c: Char): Boolean =
      peek(i) == Some(c)

    def checkOrDie(i: Int, c: Char): Unit =
      peek(i) match {
        case Some(c) =>
          ()
        case o =>
          val x = o.getOrElse("eof")
          sys.error(s"at position $i, expected '$c' but got '$x'")
      }

    def parseRe(i: Int): (Rx, Int) = {
      val (rx0, j) = parseSimple(i)
      if (check(j, '|')) {
        val (rx1, k) = parseRe(j + 1)
        (rx0 + rx1, k)
      } else {
        (rx0, j)
      }
    }

    def parseSimple(i: Int): (Rx, Int) = {
      val (rx0, j) = parseBasic(i)
      peek(j) match {
        case Some('|') | Some(')') | None =>
          (rx0, j)
        case _ =>
          val (rx1, k) = parseSimple(j)
          (rx0 * rx1, k)
      }
    }

    def parseBasic(i: Int): (Rx, Int) = {
      val (rx, j) = parseAtomic(i)
      peek(j) match {
        case Some('+') => (rx * rx.star, j + 1)
        case Some('*') => (rx.star, j + 1)
        case _ => (rx, j)
      }
    }

    def parseAtomic(i: Int): (Rx, Int) =
      peek(i) match {
        case Some('(') => parseGroup(i + 1)
        case Some('.') => (dot, i + 1)
        case Some('[') => parseSet(i + 1)
        case Some('\\') =>
          val (c, j) = parseEscaped(i + 1)
          (Rx.Letter(c), j)
        case Some(c) => (Rx.Letter(c), i + 1)
        case None => sys.error("!")
      }

    def parseEscaped(i: Int): (Char, Int) =
      if (check(i, 'u')) {
        if (s.length <= (i + 4)) {
          val t = s.substring(i + 1)
          sys.error(s"expected 4 hex digits, got '$t'")
        } else {
          val c = Integer.parseInt(s.substring(i + 1, i + 5), 16).toChar
          (c, i + 5)
        }
      }
      else (s.charAt(i), i + 1)

    def parseChar(i: Int): (Char, Int) = {
      val c = s.charAt(i)
      require(!special(c))
      if (check(c, '\\')) {
        parseEscaped(i + 1)
      } else {
        (c, i + 1)
      }
    }

    def parseItem(i: Int): (LetterSet, Int) = {
      val (c0, j) = parseChar(i)
      if (check(j, '-')) {
        val (c1, k) = parseChar(j + 1)
        (LetterSet(c0 to c1), k)
      } else {
        (LetterSet(c0 to c0), j)
      }
    }

    def parseItems(i: Int): (LetterSet, Int) = {
      val (ls0, j) = parseItem(i)
      if (check(j, ']')) {
        (ls0, j + 1)
      } else {
        val (ls1, k) = parseItems(j)
        (ls0 | ls1, k)
      }
    }

    def parseSet(i: Int): (Rx, Int) = {
      val rev = check(i, '^')
      val j = if (rev) i + 1 else i
      val (ls0, k) = parseItems(j)
      val ls = if (rev) ~ls0 else ls0
      checkOrDie(k, ']')
      (Rx.Letters(ls), k)
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
