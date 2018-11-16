package example

import org.scalatest._
import org.atnos.eff.syntax.all._
import Main._

class MainSpec extends FunSpec with Matchers {
  describe("The program") {
    it("should return a tuple with an integer and list of logs") {
      val base = 10
      val expected = 1024
      val expectedLogs = Set(
        s"the required power is ${base}",
        s"the result is ${expected}"
      )

      val (actual, logs) = program[Stack]
        .runReader(base)
        .runWriter
        .runEval
        .run

      assert(actual === expected)
      assert(expectedLogs.forall(logs.contains) === true)
    }
  }
}
