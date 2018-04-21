package tfscala.chapter1

object Chap1Module {

  import AnagramModule._
  import SongModule._

  // TODO: like to get the main out of here for hygenic reasons
  def main(args: Array[String]): Unit = {
    val wordList = List(
      "the",
      "rain",
      "in",
      "tone",
      "spain",
      "falls",
      "note",
      "mainly",
      "on",
      "no",
      "the",
      "plain")

    println(anagrams(2)(wordList))
    println("----------------------------------------")
    println(anagrams(3)(wordList))
    println("----------------------------------------")
    println(anagrams(4)(wordList))
    println("----------------------------------------")
    println(anagrams(5)(wordList))
    println("----------------------------------------")
    println(anagrams(6)(wordList))
    println("----------------------------------------")
    println("----------------------------------------")
    println("----------------------------------------")
    println(song(4))
  }
}

object SongModule {

  def song(n: Int): String = n match {
    case 0 => ""
    case _ => song(n - 1) + "\n" + verse(n)
  }

  def verse(n: Int): String =
    line1(n) +
    line2and4 +
    line3(n) +
    line2and4

  def line1(n: Int): String = n match {
    case 1 => "One man went to mow\n"
    case _ => numWordsInitCap(n - 2) + " men went to mow\n"
  }

  def line2and4: String = "Went to mow a meadow\n"

  def line3(n: Int): String = n match {
    case 1 => "One man and his dog\n"
    case _ => numWordsInitCap(n - 2) + " men, " + menList(n - 1)
  }

  def menList(n: Int): String = n match {
    case 1 => countOfMan(n) + " and his dog\n"
    case _ => countOfMan(n) + ", " + menList(n - 1)
  }

  val numWordsInitCap = Array("Two", "Three", "Four", "Five")

  val numWordsLower = Array("zero", "one", "two", "three", "four", "five")

  def manWord(n: Int): String = if (n == 1) "man" else "men"

  def countOfMan(n: Int): String = numWordsLower(n) + " " + manWord(n)








}


object AnagramModule {

  type Word = String
  type Label = String

  def anagrams(n: Int): List[Word] => String =
    ((concat _) compose
      m(showAnagram) compose
      groupByLabel compose
      sortLabels compose
      m(addLabel) compose
      wordsOfLength(n))

  def wordsOfLength(n: Int)(as: List[Word]): List[Word] = as.filter(_.length == n)

  def addLabel(a: Word): (Label, Word) = (a.sorted, a)

  def sortLabels(as: Seq[(Label, Word)]): Seq[(Label, Word)] = as.sorted

  def groupByLabel(anas: Seq[(Label, Word)]): Seq[(Label, List[Word])] =
    // TODO: need to reverse word sublists
    anas.foldLeft(List[(Label, List[Word])]())((x,y) => x match {
      case (anakey, ws) :: as if anakey == y._1 => (anakey, y._2 :: ws) :: as
      case _ => (y._1, List(y._2)) :: x
    }).reverse

  def showAnagram(a: (Label, List[Word])): String = a match {
    case (label, words) => label + ":\t" + words.mkString(", ")
  }

  def concat(xs: Seq[String]): String = xs.mkString("\n")

  // might be a better way to do this (and better name if not)
  def m[A, B](f:A => B) = (l: Seq[A]) => l.map(f)

}
