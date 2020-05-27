package antimirov
package bench

import java.util.concurrent.TimeUnit
import java.util.regex.Pattern
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class RxBenchmark {

  val Evil = """(o*)*a"""
  val EvilRx = Rx.parse(Evil)
  val EvilNfa = Nfa.fromRx(EvilRx)
  val EvilCompressedNfa = CompressedNfa.fromNfa(EvilNfa)
  val EvilJava = Pattern.compile(Evil)

  val s = "o" * 15
  @Benchmark def rxAcceptsEvil(): Boolean = EvilRx.accepts(s + "a")
  @Benchmark def rxRejectEvil(): Boolean = EvilRx.rejects(s)

  @Benchmark def cnfaAcceptsEvil(): Boolean = EvilCompressedNfa.accepts(s + "a")
  @Benchmark def cnfaRejectEvil(): Boolean = EvilCompressedNfa.rejects(s)

  @Benchmark def javaAcceptsEvil(): Boolean = EvilJava.matcher(s + "a").matches
  @Benchmark def javaRejectsEvil(): Boolean = !EvilJava.matcher(s).matches

  val Email = """[A-Za-z0-9._%+\-]+@[A-Za-z0-9.\-]+\.[A-Za-z]{2,6}"""
  val EmailRx = Rx.parse(Email)
  val EmailNfa = Nfa.fromRx(EmailRx)
  val EmailCompressedNfa = CompressedNfa.fromNfa(EmailNfa)
  val EmailJava = Pattern.compile(Email)

  @Benchmark def rxAcceptsEmail(): Boolean = EmailRx.accepts("erik@osheim.org")
  @Benchmark def rxRejectEmail(): Boolean = EmailRx.rejects("erik@osheim.org.")

  @Benchmark def cnfaAcceptsEmail(): Boolean = EmailCompressedNfa.accepts("erik@osheim.org")
  @Benchmark def cnfaRejectEmail(): Boolean = EmailCompressedNfa.rejects("erik@osheim.org.")

  @Benchmark def javaAcceptsEmail(): Boolean = EmailJava.matcher("erik@osheim.org").matches
  @Benchmark def javaRejectsEmail(): Boolean = !EmailJava.matcher("erik@osheim.org.").matches
}
