package tfscala.chapter3

object Chap3Module {

  import SqrtModule._
  // import SongModule._

  // TODO: like to get the main out of here for hygenic reasons
  def main(args: Array[String]): Unit = {
    // val wordList = List(
    //   "the",
    //   "rain",
    //   "in",
    //   "tone",
    //   "spain",
    //   "falls",
    //   "note",
    //   "mainly",
    //   "on",
    //   "no",
    //   "the",
    //   "plain")


    println(until((x:Int) => x > 100, (x:Int) => x * 7, 1))
    println("----------------------------------------")
    println(math.sqrt(4))
    println(isqrtl(4))
    println(isqrtb(4))
    println(sqrt(4))
    println("----------------------------------------")
    println(math.sqrt(42))
    println(isqrtl(42))
    println(isqrtb(42))
    println(sqrt(42))
    println("----------------------------------------")
    println(math.sqrt(502))
    println(isqrtl(502))
    println(isqrtb(502))
    println(sqrt(502))
    println("----------------------------------------")



    // println(anagrams(4)(wordList))
    // println(anagrams(5)(wordList))
    // println("----------------------------------------")
    // println(anagrams(6)(wordList))
    // println("----------------------------------------")
    // println("----------------------------------------")
    // println("----------------------------------------")
    // println(song(4))
  }
}

object SqrtModule {
  // isqrt :: Float -> Integer
  // isqrt x = until (\n -> n^2 > x) (+1) 1 - 1




  def isqrtl(x: Float): Int =
    until((n: Int) => Math.pow(n,2) > x, (n: Int) => n + 1, 1) - 1




  def isqrtb(x: Float): Int =
    until(unit, shrink(x), bound(x))._1

  def unit(interval: Tuple2[Int, Int]): Boolean = interval._1 + 1 == interval._2

  def shrink(x: Float)(interval: Tuple2[Int, Int]): Tuple2[Int, Int] = {
    val guess = (interval._1 + interval._2) / 2
    if (guess*guess <= x) (guess, interval._2) else (interval._1, guess)
  }

  def bound(x: Float): Tuple2[Int, Int] =
    (0, until((n:Int) => n*n > x, (n:Int) => n*2, 1))


  def sqrt(x: Double): Double =
    until((y: Double) => math.abs((y*y) - x) < (0.000001 * x), (y: Double) => (y + (x/y))/2, x)




  // TODO: annotate tail-recursive
  def until[A](p: A => Boolean, f: A => A,  x: A): A =
    if (p(x)) x else until(p, f, f(x))

}

object Chap3Answers {
  // A: -2 + 3 = 1
  //    3 + -2 = error
  //    3 + (-2) = 1
  //    subtract 2 3 = 1
  //    subtract 3 = fn of some type
  //    mysub = flip (-)

  // B: (^^) x n = 1 / (x ^n)
  //    --- wrong!!! ---
  //    (^^) x n = if (0 <= n) x^n else 1/(x ^ (negate n))

  // C: No, a fromInteger conversion is required

  // D: number is negative e.g. answer would be floor(-3.1) should be 4

  // E: see SqrtModule above
}
