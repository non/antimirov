package antimirov

object Graphviz {

  def dot(dfa: Dfa): String =
    dot(dfa, 'α', "Courier")

  def dot(dfa: Dfa, symbol: Char, fontName: String): String = {

    def subscript(i: Int): String =
      if (i < 10) ('\u2080' + i).toChar.toString
      else subscript(i / 10) + subscript(i % 10)

    val sb = new StringBuilder
    val font = "\"" + fontName + "\""
    sb.append("digraph g {\n")
    sb.append("  rankdir=\"LR\"\n")
    sb.append(s"  graph [fontname=$font]\n")
    sb.append(s"  node [fontname=$font]\n")
    sb.append(s"  edge [fontname=$font]\n")
    sb.append("  {\n")
    sb.append("    start [label=\"\" shape=none]\n")
    (0 until dfa.edges.length).foreach { i =>
      val shape = if (dfa.accept(i)) "doublecircle" else "circle"
      sb.append(s"    n${i} [label=${symbol}${subscript(i)} shape=$shape]\n")
    }
    sb.append("  }\n")
    sb.append("  start -> n0\n")
    dfa.edges.iterator.zipWithIndex.foreach { case (e, src) =>
      e.toLetterMap.valueMap.iterator.foreach { case (dst, ls) =>
        val k = ("\"" + ls.toString + "\"").replace("\\", "\\\\")
        sb.append(s"  n${src} -> n${dst} [label=$k]\n")
      }
    }
    sb.append("}\n")
    sb.toString
  }
}
