package antimirov

object Chars {

  val Special: Set[Char] =
    "{}[]()^$.|*+?\\∅".toSet

  val RangeSpecial: Set[Char] =
    "^[]-\\".toSet

  val Decoded: Map[Char, Char] =
    Map(
      'b' -> '\b',
      't' -> '\t',
      'n' -> '\n',
      'f' -> '\f',
      'r' -> '\r')

  val Coded: Map[Char, String] =
    Map(
      '\b' -> "\\b",
      '\t' -> "\\t",
      '\n' -> "\\n",
      '\f' -> "\\f",
      '\r' -> "\\r")

  def escape(s: String): String = escape(s, Special)

  def escape(s: String, special: Set[Char]): String = {
    val sb = new StringBuilder
    s.foreach(c => sb.append(escape(c, special)))
    sb.toString
  }

  def escape(c: Char): String = escape(c, Special)

  def escape(c: Char, special: Set[Char]): String =
    if (special(c)) s"\\$c"
    else if (' ' <= c && c <= '~') c.toString
    else Coded.getOrElse(c, "\\u%04x".format(c.toInt))

  val HexChars = "([0-9a-fA-f]{4})".r
}
