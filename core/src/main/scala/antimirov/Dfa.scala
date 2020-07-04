package antimirov

import scala.reflect.ClassTag

/**
 * DFA stands for deterministic finite-state automaton.
 *
 * Invariants:
 *  - the start ID == 0
 *  - accept.size == edges.length
 */
case class Dfa(
  accept: BitSet,
  edges: Array[Edges]) {

  /**
   * See if a particular node has an edge corresponding to a
   * particular character, and if so where it leads.
   */
  private def get(n: Int, c: Char): Option[Int] =
    edges(n).get(c)

  /**
   * Returns whether this DFA accepts the given string or not.
   */
  def accepts(s: String): Boolean = {
    var state = 0
    var i = 0
    while (i < s.length) {
      get(state, s.charAt(i)) match {
        case Some(n) => state = n
        case None => return false
      }
      i += 1
    }
    accept(state)
  }

  /**
   * Returns whether this DFA rejects the given string or not.
   */
  def rejects(s: String): Boolean =
    !accepts(s)

  /**
   * Hopcroft's DFA minimization algorithm.
   *
   * Produce a new DFA with the minimal number of states that accepts
   * the same language (i.e. the same set of strings).
   *
   * This method does not prune unreachable states. In practice the
   * DFAs we build only have states that are reachable and that can
   * lead to an accepting state. In the future we may provide a method
   * to prune states that are either unreachable from start or that
   * cannot lead to an accepting state.
   */
  def minimize: Dfa = {
    type Partition = Set[Int]

    val F: Partition = accept.toSet
    val Q: Partition = (0 until edges.length).toSet
    val Sigma: List[LetterSet] = alphabet

    var P: Set[Partition] = Set(F, Q -- F)
    var W: Set[Partition] = Set(F, Q -- F)

    while (W.nonEmpty) {
      val A = W.head
      W = W - A
      Sigma.foreach { cs =>
        val c = cs.minOption.get
        val X = Q.filter(s0 => get(s0, c).filter(A(_)).isDefined)
        P.foreach { Y =>
          val XaY = X & Y
          val YmX = Y -- X
          if (XaY.nonEmpty && YmX.nonEmpty) {
            P = P - Y + XaY + YmX
            W =
              if (W(Y)) W - Y + XaY + YmX
              else if (XaY.size <= YmX.size) W + XaY
              else W + YmX
          }
        }
      }
    }

    val m: Map[Int, Int] =
      P.iterator.flatMap { p =>
        if (p.nonEmpty) {
          val m = p.min
          p.iterator.map(n => (n, m))
        } else Iterator.empty
      }.toMap

    val bldr = new Dfa.Builder[Int](0)
    var i = 0
    while (i < edges.length) {
      val x = m.getOrElse(i, i)
      if (x == i) {
        bldr.addNode(x, accept(x))
        edges(i).iterator.foreach { case ((c1, c2), j) =>
          val y = m.getOrElse(j, j)
          bldr.addNode(y, accept(y))
          bldr.addEdge(x, LetterSet(c1 to c2), y)
        }
      }
      i += 1
    }
    bldr.build
  }

  /**
   * Print a string representation the DFA.
   *
   * The starting state (0) is printed first. Any accepting state is
   * printed surrounded by brackets (e.g. [4]).
   */
  override def toString: String =
    edges.iterator.zipWithIndex.map { case (e, i) =>
      val label = if (accept(i)) s"[$i]" else s"$i"
      e.toLetterMap.valueMap.iterator.map { case (dst, ls) =>
        ls.toString match {
          case s if s.startsWith("[") => s"$s -> $dst"
          case s => s"'$s' -> $dst"
        }
      }.mkString(s"$label -> {", ", ", "}")
    }.mkString("\n")

  /**
   * Return the alphabet of this DFA, represented as a list of
   * character sets. Each character set can be treated as a single
   * logical character for the purposes of this DFA.
   */
  private def alphabet: List[LetterSet] = {
    // def keySets(m: LetterMap[Int]): List[LetterSet] =
    //   m.iterator.map { case ((c1, c2), _) => LetterSet(c1 to c2) }.toList
    def keySets(e: Edges): List[LetterSet] =
      e.iterator.map { case ((c1, c2), _) => LetterSet(c1 to c2) }.toList
    edges.iterator.map(keySets).toList match {
      case h :: t =>
        t.foldLeft(h)((x, y) => LetterSet.venn(x, y).map(_.value))
      case Nil =>
        Nil
    }
  }
}

object Dfa {

  class Builder[S: ClassTag](val start: S) {

    override def toString: String = s"Builder($start, $acceptSet, $edges)"

    var acceptSet: Set[S] =
      Set.empty

    var edges: Map[S, LetterMap[S]] =
      Map(start -> LetterMap.empty[S])

    def addNode(node: S, accepting: Boolean = false): Unit = {
      if (accepting) acceptSet += node
      if (edges.contains(node)) return ()
      edges = edges.updated(node, LetterMap.empty[S])
    }

    def addEdge(src: S, keys: LetterSet, dst: S): Unit = {
      addNode(src)
      addNode(dst)
      edges = edges.updated(src, edges(src) ++ LetterMap(keys, dst))
    }

    def build: Dfa = {
      val vi = new ValueIndex[S]
      require(vi.indexOf(start) == 0) // ensure start is 0
      val count = edges.size
      val accept1 = BitSet(count, acceptSet.map(vi.indexOf))
      //val edges1 = new Array[LetterMap[Int]](count)
      val edges1 = new Array[Edges](count)
      edges.foreach { case (src, letterMap) =>
        edges1(vi.indexOf(src)) = Edges(letterMap.mapValues(vi.indexOf))
      }
      Dfa(accept1, edges1)
    }
  }
}
