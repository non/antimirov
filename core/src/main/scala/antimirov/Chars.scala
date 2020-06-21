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

  val HexChars = "([0-9a-fA-F]{4})".r

  /**
   * Escape a character using the standard set of special regex
   * characters.
   */
  def escape(c: Char): String =
    escape(c, Special)

  /**
   * Escape a character using a given set of special characters.
   */
  def escape(c: Char, special: Set[Char]): String =
    if (Coded.contains(c)) Coded(c)
    else if (special(c)) s"\\$c"
    else if (' ' <= c && c <= '~') c.toString
    else Coded.getOrElse(c, "\\u%04x".format(c.toInt))

  /**
   * Escape a string using the standard set of special regex
   * characters. This also wraps the string in double-quotes.
   */
  def escape(s: String): String =
    escape(s, Special + '"')

  /**
   * Escape a string using the given set of special characters. This
   * also wraps the string in double-quotes.
   */
  def escape(s: String, special: Set[Char]): String = {
    val sb = new StringBuilder
    sb.append("\"")
    s.foreach(c => sb.append(escape(c, special)))
    sb.append("\"")
    sb.toString
  }

  /**
   * Parse a string escaped with `Chars.escape(s)`.
   */
  def unescape(s: String): Option[String] =
    unescape(s, Special + '"')

  /**
   * Parse a string escaped with `Chars.escape(s, special)`.
   */
  def unescape(s: String, special: Set[Char]): Option[String] = {
    if (s.length < 2) return None
    if (s.charAt(0) != '"') return None
    if (s.charAt(s.length - 1) != '"') return None

    val sb = new StringBuilder
    var i = 1
    val limit = s.length - 1
    while (i < limit) {
      val c = s.charAt(i)
      c match {
        case '\\' =>
          val cc = s.charAt(i + 1)
          if (Decoded.contains(cc)) {
            sb.append(Decoded(cc))
            i += 2
          } else if (special(cc)) {
            sb.append(cc)
            i += 2
          } else if (cc == 'u') {
            if (i + 6 >= s.length) return None
            try {
              val n = Integer.parseInt(s.substring(i + 2, i + 6), 16)
              sb.append(n.toChar)
              i += 6
            } catch { case _: NumberFormatException =>
              return None
            }
          } else {
            return None
          }
        case c =>
          sb.append(c)
          i += 1
      }
    }
    Some(sb.toString)
  }
}
