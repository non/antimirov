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
   *
   */
  def followAll(st0: BitSet): LetterMap[BitSet] =
    edges.mapValues { array =>
      var i = 0
      val st1 = BitSet.empty(size)
      while (i < array.length) {
        if (st0(i)) {
          val bs = array(i)
          if (bs != null) st1 |= bs
        }
        i += 1
      }
      st1
    }

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

  def toDfa: Dfa =
    Dfa.fromNfa(this)
}

object Nfa {

  /**
   * Build an NFA from a given regular expression.
   */
  def fromRx(r: Rx): Nfa =
    NfaBuilder.fromRx(r).build

  object NfaBuilder {

    def alloc(start: Int, end: Int): NfaBuilder =
      NfaBuilder(start, end, Map(start -> Map.empty, end -> Map.empty))

    def fromRx(rx: Rx): NfaBuilder = {
      val index = new Index()
      def recur(r: Rx): NfaBuilder =
        r match {
          case Rx.Phi =>
            NfaBuilder.alloc(index.next(), index.next())
          case Rx.Empty =>
            val n = index.next()
            NfaBuilder.alloc(n, n)
          case Rx.Letter(c) =>
            val n0 = index.next()
            val n1 = index.next()
            NfaBuilder.alloc(n0, n1).addEdge(n0, Some(LetterSet(c)), n1)
          case Rx.Letters(cs) =>
            val n0 = index.next()
            val n1 = index.next()
            NfaBuilder.alloc(n0, n1).addEdge(n0, Some(cs), n1)
          case Rx.Concat(r1, r2) =>
            val nfa1 = recur(r1)
            val nfa2 = recur(r2)
            NfaBuilder.alloc(nfa1.start, nfa2.accept)
              .absorb(nfa1)
              .absorb(nfa2)
              .addEdge(nfa1.accept, None, nfa2.start)
          case Rx.Choice(r1, r2) =>
            val start = index.next()
            val nfa1 = recur(r1)
            val nfa2 = recur(r2)
            val accept = index.next()
            NfaBuilder.alloc(start, accept)
              .absorb(nfa1)
              .absorb(nfa2)
              .addEdge(start, None, nfa1.start)
              .addEdge(start, None, nfa2.start)
              .addEdge(nfa1.accept, None, accept)
              .addEdge(nfa2.accept, None, accept)
          case Rx.Star(r) =>
            val nfa = recur(r)
            nfa
              .addEdge(nfa.start, None, nfa.accept)
              .addEdge(nfa.accept, None, nfa.start)
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

  case class NfaBuilder(
    start: Int,
    accept: Int,
    edges: Map[Int, Map[Option[LetterSet], Set[Int]]]) {

    def addEdge(from: Int, c: Option[LetterSet], to: Int): NfaBuilder =
      NfaBuilder(start, accept, edges.get(from) match {
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

    def absorb(nfa: NfaBuilder): NfaBuilder =
      nfa.transitions.foldLeft(this) {
        case (nfa, (from, c, to)) => nfa.addEdge(from, c, to)
      }

    def replace(before: Int, after: Int): NfaBuilder = {
      val accept2 = if (accept == after) before else accept
      NfaBuilder(start, accept2, edges.flatMap { case (k, m) =>
        if (k == before) {
          None
        } else {
          val k2 = if (k == after) before else k
          Some((k2, m.map { case (ok, ov) =>
            (ok, ov.map(x => if (x == after) before else x))
          }))
        }
      })
    }

    private def epsilonOnly(n: Int): Option[Int] =
      edges.get(n).filter(_.size == 1).flatMap(_.get(None).map(_.head))

    def compress: NfaBuilder = {
      def loop(bldr: NfaBuilder, queue: List[Int], seen: Set[Int]): NfaBuilder =
        queue match {
          case Nil =>
            bldr
          case n :: rest if seen(n) =>
            loop(bldr, rest, seen)
          case n :: rest =>
            bldr.epsilonOnly(n) match {
              case Some(nn) =>
                loop(bldr.replace(n, nn), rest, seen)
              case None =>
                val ids = bldr.edges.get(n).toList.flatMap(_.values.flatMap(_.toList).toList)
                loop(bldr, ids ::: rest, seen + n)
            }
        }
      loop(this, List(start), Set.empty)
    }

    /**
     * Build an Nfa from this NfaBuilder instance.
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
