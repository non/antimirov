package antimirov

import org.scalacheck.{Gen, Prop, Properties}

import Prop.{forAllNoShrink => forAll}

object TimingProperties {

  import java.util.concurrent.{Callable, ForkJoinPool, FutureTask}

  val pool = new ForkJoinPool()

  def timed(label: => String, warnMs: Long, abortMs: Long, failOnAbort: Boolean, body: => Prop): Prop = {

    val task = new FutureTask(new Callable[Prop] { def call(): Prop = body })
    pool.execute(task)

    val start = System.nanoTime()
    val warnT = start + warnMs * 1000000L
    val abortT = start + abortMs * 1000000L

    var result: Option[Prop] = None
    var t0 = start
    while (!task.isDone && t0 < abortT) {
      Thread.sleep(0L, 100000)
      val t1 = System.nanoTime()
      t0 = t1
    }

    if (task.isDone) {
      result = Some(task.get)
    } else {
      task.cancel(true)
    }

    val now = System.nanoTime()
    val duration = (now - start).toDouble / 1000000.0

    if (result.isEmpty) {
      val word = if (failOnAbort) "FAIL" else "STOP"
      println(s"$word {$label} failed to finish within ${abortMs}ms")
    } else if (now >= warnT) {
      println("WARN {%s} took %.1fms (longer than %dms)".format(label, duration, warnMs))
    }

    result.getOrElse(!failOnAbort)
  }

}

trait TimingProperties { self: Properties =>

  import TimingProperties.timed

  def enableTiming: Boolean = true
  def failOnAbort: Boolean = true

  def scale: Long = 10
  def warnMs: Long = 50L * scale
  def abortMs: Long = 100L * scale

  def timedProp[A](name: String, ga: Gen[A])(f: A => Prop): Unit = {
    self.property(name) =
      if (enableTiming) {
        forAll(ga)(a => timed(s"$name : $a", warnMs, abortMs, failOnAbort, f(a)))
      } else {
        forAll(ga)(f)
      }
    ()
  }

  def timedProp[A, B](name: String, ga: Gen[A], gb: Gen[B])(f: (A, B) => Prop): Unit = {
    self.property(name) =
      if (enableTiming) {
        forAll(ga, gb)((a, b) => timed(s"$name : $a : $b", warnMs, abortMs, failOnAbort, f(a, b)))
      } else {
        forAll(ga, gb)(f)
      }
    ()
  }

  def timedProp[A, B, C](name: String, ga: Gen[A], gb: Gen[B], gc: Gen[C])(f: (A, B, C) => Prop): Unit = {
    self.property(name) =
      if (enableTiming) {
        forAll(ga, gb, gc)((a, b, c) => timed(s"$name : $a : $b : $c", warnMs, abortMs, failOnAbort, f(a, b, c)))
      } else {
        forAll(ga, gb, gc)(f)
      }
    ()
  }
}
