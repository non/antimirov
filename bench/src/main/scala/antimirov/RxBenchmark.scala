package antimirov
package bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class RxBenchmark {

  val Email = Rx.parse("""[A-Za-z0-9._%+\-]+@[A-Za-z0-9.\-]+\.[A-Za-z]{2,6}""")

  val EmailJava = Email.toJava

  @Benchmark
  def accepts(): Boolean =
    Email.accepts("erik@osheim.org")

  @Benchmark
  def reject(): Boolean =
    Email.rejects("erik@osheim.org.")

  @Benchmark
  def acceptsJava(): Boolean =
    EmailJava.matcher("erik@osheim.org").matches

  @Benchmark
  def rejectJava(): Boolean =
    !EmailJava.matcher("erik@osheim.org.").matches

}
