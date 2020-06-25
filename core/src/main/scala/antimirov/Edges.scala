package antimirov

sealed abstract class Edges {

  def get(c: Char): Option[Int]
  def toLetterMap: LetterMap[Int]
  def iterator: Iterator[((Char, Char), Int)] = toLetterMap.iterator

  def mapValues(f: Int => Int): Edges =
    Edges(toLetterMap.mapValues(f))
}

object Edges {

  def apply(lm: LetterMap[Int]): Edges =
    if (lm.isEmpty) {
      NoEdge
    } else if (lm.size == 1) {
      val ((c, _), n) = lm.iterator.next
      SingleEdge(c, n)
    } else if (lm.maxKeyOption.get < 256) {
      val array = Array.fill(256)(-1)
      lm.items.foreach { case (c, n) => array(c.toInt) = n }
      AsciiEdges(array, lm)
    } else if (lm.size <= 64) {
      MapEdges(lm.toMap, lm)
    } else {
      lm.iterator.next match {
        case (('\u0000', '\uffff'), n) => AllEdges(n)
        case _ => LetterMapEdges(lm)
      }
    }

  case object NoEdge extends Edges {
    def get(c: Char): Option[Int] = None
    val toLetterMap = LetterMap.empty[Int]
  }

  case class SingleEdge(char: Char, dest: Int) extends Edges {
    val result = Some(dest)
    def get(c: Char): Option[Int] = if (c == char) result else None
    val toLetterMap = LetterMap(char, dest)
  }

  case class AllEdges(dest: Int) extends Edges {
    val result = Some(dest)
    def get(c: Char): Option[Int] = result
    val toLetterMap = LetterMap(LetterSet.Full, dest)
  }

  case class AsciiEdges(array: Array[Int], toLetterMap: LetterMap[Int]) extends Edges {
    def get(c: Char): Option[Int] = {
      if (c >= 256) return None
      val x = array(c.toInt)
      if (x >= 0) return Some(x) else None
    }
  }

  case class MapEdges(m: Map[Char, Int], toLetterMap: LetterMap[Int]) extends Edges {
    def get(c: Char): Option[Int] = m.get(c)
  }

  case class LetterMapEdges(toLetterMap: LetterMap[Int]) extends Edges {
    def get(c: Char): Option[Int] = toLetterMap.get(c)
  }
}
