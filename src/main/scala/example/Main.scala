package example

import cats._, data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object Main {
  type Stack = Fx.fx3[
    Writer[String, ?],
    Reader[Int, ?],
    Eval
  ]
  type _readerInt[R] = Reader[Int, ?] |= R
  type _writerString[R] = Writer[String, ?] |= R

  // the program that consists of effects of the stack
  def program[R: _readerInt: _writerString: _eval]: Eff[R, Int] =
    for {
      // get the configuration
      n <- ask[R, Int]

      // log the current configuration value
      _ <- tell(s"the required power is ${n}")

      // compute the nth power of 2
      a <- delay(math.pow(2, n.toDouble).toInt)

      // log the result
      _ <- tell(s"the result is ${a}")
    } yield a

  def main(args: Array[String]): Unit = {
    println(
      program[Stack]
        .runReader(10)
        .runWriter
        .runEval
        .run
    )
  }
}
