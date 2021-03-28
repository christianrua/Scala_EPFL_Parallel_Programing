package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def auxBalance(remainChars: Array[Char], bracketAcc: Int): Boolean = {
      if (bracketAcc < 0) false
      else if (remainChars.length == 0) bracketAcc == 0
      else remainChars.head match {
        case '(' => auxBalance(remainChars.tail, bracketAcc + 1)
        case ')' => auxBalance(remainChars.tail, bracketAcc - 1)
        case _ => auxBalance(remainChars.tail, bracketAcc)
      }
    }
    auxBalance(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  final case class ExtBrack(extL: Int, extR: Int) {
    def combine(that: ExtBrack): ExtBrack = {
      // that should be extra brackets from a sequence of chars at the RHS of the array
      val extras = this.extL - that.extR
      if (extras > 0) ExtBrack(that.extL + Math.abs(extras), this.extR)
      else ExtBrack(that.extL, this.extR + Math.abs(extras))
    }
  }

  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, extL: Int, extR: Int):ExtBrack = {
      if (idx == until) ExtBrack(extL, extR)
      else chars(idx) match {
        case '(' => traverse(idx + 1, until, extL + 1, extR)
        case ')' => if (extL > 0) traverse(idx + 1, until, extL - 1, extR)
        else traverse(idx + 1, until, extL, extR + 1)
        case _ => traverse(idx + 1, until, extL, extR)
      }
    }

    def reduce(from: Int, until: Int): ExtBrack = {
      if (until - from <= threshold) traverse(from, until, 0, 0) // sequential traverse
      else {
        val mid = (from + until) / 2
        val (left, right) = parallel(reduce(from, mid), reduce(mid, until))
        left combine right
      } // parallel reduce, then combining them in some way
    }

    reduce(0, chars.length) == ExtBrack(0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
