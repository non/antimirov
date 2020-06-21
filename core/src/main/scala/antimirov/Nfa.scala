package antimirov

/**
 * NFA stands for Non-deterministic finite-state automaton.
 *
 * NFAs allow non-determinism in their graphs: epsilon transitions
 * which can be taken (or not taken) without any corresponding input.
 * This means there are potentially many paths through an NFA which
 * need to be explored.
 *
 * This NFA implementation is an optimized form that uses a bitset to
 * represent the possible states we are in at any given time. When we
 * read characters we compute a new bitset of all the possible states
 * we could get to from any of our previous states. This means we
 * never have to backtrack, at the cost of the overhead of computing
 * these sets (rather than greedily following one path).
 *
 * The size of the NFA (n) is the number of NFA states it contains.
 * Since our physical states are sets of n bits, this means that we
 * could potentially have up to 2^n different bitsets (although in
 * practice most of these are unreachable). When compiling an NFA into
 * a DFA, we will need to reify each "reachable" bitset as its own DFA
 * state, which again means that for n NFA states, we might have up to
 * 2^n DFA states.
 *
 * We use LetterMap to determine which transition (if any) to take.
 * This introduces a log(k) cost to each transition (where k is the
 * number of distinct ranges in the LetterMap), but has the advantage
 * that it is simple to implement and can handle even very large and
 * complex ranges of character inputs.
 */
case class Nfa(
  size: Int,
  start: BitSet,
  accept: BitSet,
  edges: LetterMap[Array[BitSet]]) {

  override def toString: String = {
    val e = edges.mapValues {
      case null => "null"
      case arr => arr.iterator.map {
        case null => "null"
        case bs => bs.toString
      }.mkString("Array(", ", ", ")")
    }
    s"Nfa($size, $start, $accept, $e)"
  }

  /**
   * Return true if (and only if) this NFA matches the given string.
   */
  def accepts(s: String): Boolean = {
    var i = 0
    var st = start
    while (i < s.length) {
      follow(st, s.charAt(i)) match {
        case Some(st1) => st = st1
        case None => return false
      }
      i += 1
    }
    st intersects accept
  }

  /**
   * Return true if (and only if) this NFA does not match the given
   * string.
   */
  def rejects(s: String): Boolean =
    !accepts(s)

  /**
   * Given a bitset of current states and a character of input,
   * compute the bitset of future states (if any).
   *
   * This method will return None when we failed to match the input,
   * and will return Some(_) when we have a bitset of states to
   * continue with. There is no check that the bitset is non-empty,
   * although based on the NFA is constructed it should be impossible
   * to end up with an empty bitset.
   *
   * (In any case, an empty bitset would not lead to bugs other than
   * slower-than-necessary rejection of an input.)
   */
  def follow(st0: BitSet, c: Char): Option[BitSet] =
    edges.get(c) match {
      case Some(array) =>
        val st1 = BitSet.empty(size)
        var i = 0
        val array0 = st0.array
        while (i < array0.length) {
          var raw = array0(i)
          var j = 0
          while (j < 32) {
            if ((raw & 1) == 1) {
              val n = (i << 5) + j
              val bs = array(n)
              if (bs != null) st1 |= bs
            }
            raw = raw >>> 1
            j += 1
          }
          i += 1
        }
        Some(st1)
      case None =>
        None
    }
}

object Nfa {

  object Builder {

    def alloc(start: Int, end: Int): Builder =
      Builder(start, end, Map(start -> Map.empty, end -> Map.empty))

    def fromRx(rx: Rx): Builder = {
      val index = new Index()
      def recur(r: Rx): Builder =
        r match {
          case Rx.Phi =>
            Builder.alloc(index.next(), index.next())
          case Rx.Empty =>
            val n = index.next()
            Builder.alloc(n, n)
          case Rx.Letter(c) =>
            val n0 = index.next()
            val n1 = index.next()
            Builder.alloc(n0, n1).addEdge(n0, Some(LetterSet(c)), n1)
          case Rx.Letters(cs) =>
            val n0 = index.next()
            val n1 = index.next()
            Builder.alloc(n0, n1).addEdge(n0, Some(cs), n1)
          case Rx.Concat(r1, r2) =>
            val nfa1 = recur(r1)
            val nfa2 = recur(r2)
            Builder.alloc(nfa1.start, nfa2.accept)
              .absorb(nfa1)
              .absorb(nfa2)
              .addEdge(nfa1.accept, None, nfa2.start)
          case Rx.Choice(r1, r2) =>
            val start = index.next()
            val nfa1 = recur(r1)
            val nfa2 = recur(r2)
            val accept = index.next()
            Builder.alloc(start, accept)
              .absorb(nfa1)
              .absorb(nfa2)
              .addEdge(start, None, nfa1.start)
              .addEdge(start, None, nfa2.start)
              .addEdge(nfa1.accept, None, accept)
              .addEdge(nfa2.accept, None, accept)
          case Rx.Star(r) =>
            val start = index.next()
            val nfa = recur(r)
            val accept = index.next()
            Builder.alloc(start, accept)
              .absorb(nfa)
              .addEdge(start, None, nfa.start)
              .addEdge(nfa.accept, None, accept)
              .addEdge(start, None, accept)
              .addEdge(accept, None, start)
          case Rx.Repeat(r, x, y) if x > 0 =>
            recur(Rx.Concat(r, Rx.Repeat(r, x - 1, y - 1)))
          case Rx.Repeat(r, _, y) if y > 0 =>
            recur(Rx.Choice(Rx.Empty, Rx.Concat(r, Rx.Repeat(r, 0, y - 1))))
          case Rx.Repeat(r, _, _) =>
            recur(Rx.Empty)
          case v @ Rx.Var(_) =>
            // $COVERAGE-OFF$
            sys.error(s"illegal var node found: $v")
            // $COVERAGE-ON$
        }
      recur(rx)
    }
  }

  case class Builder(
    start: Int,
    accept: Int,
    edges: Map[Int, Map[Option[LetterSet], Set[Int]]]) {

    override def toString: String = {
      edges.toList.sortBy(_._1).map { case (k, m) =>
        val s = (k == start, k == accept) match {
            case (true, true) => s"([$k])"
            case (true, false) => s"($k)"
            case (false, true) => s"[$k]"
            case (false, false) => s"$k"
          }
        s"$s -> $m"
      }.mkString("\n")
    }

    def addEdge(from: Int, c: Option[LetterSet], to: Int): Builder =
      Builder(start, accept, edges.get(from) match {
        case None =>
          edges.updated(from, Map(c -> Set(to)))
        case Some(m) =>
          m.get(c) match {
            case None =>
              edges.updated(from, m.updated(c, Set(to)))
            case Some(sts) =>
              edges.updated(from, m.updated(c, sts + to))
          }
      })

    def closure(from: Set[Int]): Set[Int] = {
      def follow(from: Int, c: Option[LetterSet]): Set[Int] =
        edges.get(from).flatMap(_.get(c)).getOrElse(Set.empty)
      def loop(s0: Set[Int]): Set[Int] = {
        val s1 = s0 | s0.flatMap(s => follow(s, None))
        if (s1 == s0) s0 else loop(s1)
      }
      loop(from)
    }

    def transitions: Iterator[(Int, Option[LetterSet], Int)] =
      for {
        pair0 <- edges.iterator
        (from, m) = pair0
        pair1 <- m.iterator
        (c, set) = pair1
        to <- set.iterator
      } yield (from, c, to)

    def absorb(nfa: Builder): Builder =
      nfa.transitions.foldLeft(this) {
        case (nfa, (from, c, to)) => nfa.addEdge(from, c, to)
      }

    private def mergeAll(it: Iterator[LetterMap[Set[Int]]]): LetterMap[Set[Int]] =
      if (it.hasNext) {
        val x = it.next
        it.foldLeft(x)((acc, m) => acc.merge(m)(_ | _))
      } else {
        LetterMap.empty
      }

    def follow(src: Int): LetterMap[Set[Int]] =
      edges.get(src) match {
        case Some(m) =>
          mergeAll(m.iterator.flatMap {
            case (Some(cs), dsts) => Some(LetterMap(cs, dsts))
            case (None, _) => None
          })
        case None =>
          LetterMap.empty
      }

    def followAll(set: Set[Int]): LetterMap[Set[Int]] =
      mergeAll(set.iterator.map(follow(_)))

    /**
     *
     */
    def toDfaBuilder: Dfa.Builder[Set[Int]] = {
      val st = closure(Set(start))
      val bldr = new Dfa.Builder(st)
      var queue: List[Set[Int]] = List(st)
      var seen: Set[Set[Int]] = Set.empty
      while (queue.nonEmpty) {
        val st0 = queue.head
        queue = queue.tail
        if (!seen(st0)) {
          seen = seen + st0
          bldr.addNode(st0, st0(accept))
          followAll(st0).iterator.foreach { case ((c1, c2), st1x) =>
            val st1 = closure(st1x)
            bldr.addNode(st1, st1(accept))
            bldr.addEdge(st0, LetterSet(c1 to c2), st1)
            queue = st1 :: queue
          }
        }
      }
      bldr
    }

    /**
     * Build an Nfa from this Builder instance.
     *
     * This operation is fairly expensive: we will need to compute the
     * transition closure of each input (which we represent as an
     * array of bitsets).
     */
    def build: Nfa = {
      val nfa = this
      val size: Int = nfa.edges.size

      val index = new ValueIndex[Int]
      index.initialize(nfa.edges.keys)
      import index.indexOf

      def clos(set: Set[Int]): Set[Int] =
        nfa.closure(set).map(indexOf(_))

      val start = BitSet(size, clos(Set(nfa.start)))
      val accept = BitSet(size, List(indexOf(nfa.accept)))

      val it: Iterator[LetterMap[Array[BitSet]]] =
        nfa.edges.iterator.map { case (k, m) =>
          val idx = indexOf(k)
          m.iterator.map {
            case (Some(cs), set) if set.nonEmpty =>
              val arr = new Array[BitSet](size)
              arr(idx) = BitSet(size, clos(set))
              LetterMap(cs, arr)
            case _ =>
              LetterMap.empty[Array[BitSet]]
          }.foldLeft(LetterMap.empty[Array[BitSet]])(_ ++ _)
        }

      def f(xs: Array[BitSet], ys: Array[BitSet]): Array[BitSet] = {
        var i = 0
        val out = new Array[BitSet](size)
        while (i < ys.length) {
          val y = ys(i)
          val x = xs(i)
          out(i) = (x != null, y != null) match {
            case (true, true) => x | y
            case (true, false) => x
            case (false, true) => y
            case (false, false) => null
          }
          i += 1
        }
        out
      }

      val edges = it.foldLeft(LetterMap.empty[Array[BitSet]]) { (acc, xs) =>
        acc.merge(xs)(f)
      }

      Nfa(size, start, accept, edges)
    }
  }
}
