package antimirov

// start = 0
// size = edges.length
case class Dfa(
  accept: BitSet,
  edges: Array[LetterMap[Int]]) {

  override def toString: String =
    edges.iterator.zipWithIndex.map { case (lm, i) =>
      val label = if (accept(i)) s"[$i]" else s"$i"
      s"$label -> $lm"
    }.mkString("\n")

  def accepts(s: String): Boolean = {
    var state = 0
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      edges(state).get(c) match {
        case Some(n) => state = n
        case None => return false
      }
      i += 1
    }
    accept(state)
  }

  def rejects(s: String): Boolean =
    !accepts(s)
}

object Dfa {

  def fromNfa(nfa: Nfa): Dfa = {
    var bldr: DfaBuilder = DfaBuilder(0, nfa.accept, Map.empty, Set.empty, Map.empty)
    var queue: List[BitSet] = List(nfa.start)
    var seen: Set[BitSet] = Set.empty
    while (queue.nonEmpty) {
      val st0 = queue.head
      queue = queue.tail
      if (!seen(st0)) {
        seen = seen + st0
        bldr = bldr.addNode(st0)
        nfa.followAll(st0).iterator.foreach { case ((c1, c2), st1) =>
          bldr = bldr.addEdge(st0, LetterSet(c1 to c2), st1)
          queue = st1 :: queue
        }
      }
    }
    bldr.build
  }

  // start = 0
  case class DfaBuilder(
    count: Int,
    acceptBitSet: BitSet,
    index: Map[BitSet, Int],
    acceptSet: Set[Int],
    edges: Map[Int, LetterMap[Int]]
  ) {

    def addNode(st: BitSet): DfaBuilder =
      if (index.contains(st)) {
        this
      } else {
        val idx = count
        val count1 = count + 1
        val index1 = index.updated(st, idx)
        val accepting = st intersects acceptBitSet
        val acceptSet1 = if (accepting) acceptSet + idx else acceptSet
        DfaBuilder(count1, acceptBitSet, index1, acceptSet1, edges)
      }

    def addEdge(src: BitSet, keys: LetterSet, dst: BitSet): DfaBuilder = {
      val b1 = this.addNode(src)
      val b2 = b1.addNode(dst)
      val (n1, n2) = (b2.index(src), b2.index(dst))
      val rm = LetterMap(keys, n2)
      val edges1 = b2.edges.updated(n1, edges.get(n1) match {
        case Some(lm) =>
          lm.merge(rm)((x, y) => if (x == y) x else sys.error(s"merging $lm and $rm failed ($x != $y)"))
        case None =>
          rm
      })
      b2.copy(edges = edges1)
    }

    def build: Dfa = {
      val accept1 = BitSet(count, acceptSet)
      val edges1 = new Array[LetterMap[Int]](count)
      var i = 0
      while (i < count) {
        edges1(i) = edges.getOrElse(i, LetterMap.empty)
        i += 1
      }
      Dfa(accept1, edges1)
    }
  }
}
