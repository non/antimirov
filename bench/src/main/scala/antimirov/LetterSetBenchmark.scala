package antimirov
package bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class LetterSetBenchmark {

  //@Param(Array("ascii", "unicode"))
  @Param(Array("ascii"))
  var mode: String = _

  //@Param(Array("10", "20", "40"))
  @Param(Array("20"))
  var size: Int = _

  var r: Random = _
  var chars: Array[Char] = _
  var strs: Array[String] = _
  var csets: Array[Set[Char]] = _
  var lsets: Array[LetterSet] = _

  def gench(mode: String): Char =
    mode match {
      case "ascii" => r.nextPrintableChar
      case "unicode" => r.nextInt(65536).toChar
      case _ => sys.error(s"unknown mode: $mode")
    }

  def genstr(mode: String): String = {
    val n = (r.nextGaussian() * size + size).abs.toInt
    (1 to n).map(_ => gench(mode).toString).mkString
  }

  @Setup
  def setup(): Unit = {
    r = new Random(s"determinism($mode, $size)".hashCode)
    chars = (1 to 100).map(_ => gench(mode)).toArray
    strs = (1 to 100).map(_ => genstr(mode)).toArray
    csets = strs.map(s => s.toSet)
    lsets = strs.map(s => LetterSet(s: _*))
  }

  @Benchmark
  def makeCSets(): Array[Set[Char]] =
    strs.map(_.toSet)

  @Benchmark
  def makeLSets(): Array[LetterSet] =
    strs.map(s => LetterSet(s: _*))

  @Benchmark
  def unionCSets(): Set[Char] =
    csets.reduceLeft(_ | _)

  @Benchmark
  def unionLSets(): LetterSet =
    lsets.reduceLeft(_ | _)

  @Benchmark
  def applyCSets(): Int =
    csets.foldLeft(0) { (n, cs) =>
      chars.foldLeft(n) { (nn, c) =>
        if (cs(c)) nn + 1 else nn
      }
    }

  @Benchmark
  def applyLSets(): Int =
    lsets.foldLeft(0) { (n, ls) =>
      chars.foldLeft(n) { (nn, c) =>
        if (ls.contains(c)) nn + 1 else nn
      }
    }
}
