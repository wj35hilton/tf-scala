package tfscala.chapter2

object Chap2Module {
  import ModModule._
  import ShowDateModule._
  import AddSumModule._

  def main(args: Array[String]): Unit = {
    println("----------------------------------------")
    println(modernise("The morphology of prex - an essay in meta-algorithmics"))
    println(modernise2("The morphology of prex - an essay in meta-algorithmics"))
    println("----------------------------------------")
    println(showDate((12,1,2016)));
    println(showDate((1,2,2003)));
    println(showDate((3,3,2013)));
    println(showDate((5,4,2013)));
    println(showDate((5,5,2014)));
    println(showDate((7,11,2014)));
    println(showDate((7,21,2014)));
    println(showDate((7,31,2014)));
    println(showDate((8,12,2014)));
    println(showDate((8,22,2014)));
    println("----------------------------------------")
    println(addSum("12345678"));
    println(addSum("00000000"));
    println(addSum("00000001"));
    println(addSum("11111111"));
    println(valid("1111111108"));
    println(valid("1111111180"));
    println(valid("0000000000"));
    println(valid("0000000001"));
    println(valid("0000000101"));
    println(valid("0000000120"));
    println(valid("1234567836"));
    println(valid("1234567863"));
  }
}

object ModModule {
  def modernise(s: String): String = s.split(" ").map(initCap).mkString(" ")

  val modernise2: String => String =
    (unwords _) compose m(initCap) compose words

  def unwords(ss: Seq[String]): String = ss.mkString(" ")

  def words(s: String): Seq[String] = s.split(" ")

  def initCap(s: String): String = s match {
    case c s_+: cs => c.toUpper +: cs
    case _ => s
  }

  def m[A, B](f:A => B) = (l: Seq[A]) => l.map(f)

  object s_+: {
    def unapply(s: String): Option[(Char, String)] = s.headOption.map{ (_, s.tail) }
  }
}


object ShowDateModule {
  type SimpleDate = Tuple3[Int, Int, Int]

  def showDate(d: SimpleDate): String = d match {
    case (m, d, y) => months(m) + " " + day(d) + "," + y
  }

  val months = Array("N/A", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

  def day(d: Int): String = d + daySuffix(d)

  def daySuffix(d: Int): String =
    if (d == 1 || d == 21 || d == 31)
      "st"
    else if (d == 2 || d == 22)
      "nd"
    else if (d == 3 || d == 23)
      "rd"
    else
      "th"
}

object AddSumModule {
  def addSum(s: String): String = f"$s%s${sum(s)}%02d"
  def valid(s: String): Boolean = sum(s.substring(0,8)) == s.substring(8, 10).toInt

  def sum(s:String): Int = s.foldLeft(0)((a, x) => a + x.asDigit)
}

object Chap2Answers {
  // Ex B
  // [0,1) -- not valid
  // double -3 -- not sure
  // double (-3) valid type is Integer
  // double double 0 ... hmmm order of binding not valid
  // if 1==0 then 2==1 not valid (no else ... 2==1 should return false though)
  // "++" == "+" ++ "+" (valid, value is true:Boolean)
  //[(+),(-)] (List[Num -> Num])
  // [[],[[]],[[[]]]] => valid triple nested list of undefined ??
  // concat ["tea", "for", '2'] nope (list can't be of mixed type)
  // concat ["tea", "for", "2"] ....

  // Ex C.
  // see function

  // Ex D.
  // Eager: f evaluated n times
  // Lazy: 1
  // f(head xs)
  // first p xs | null xs = error "Empty list"
  //            | p x = x
  //            | otherwise = first p tail xs
  // where x = head xs
  //
  // head . filter p . map f
  //

  // Ex E.
  // EAGER:
  // first p xs | null xs = Nothing
  //            | p x = Just x
  //            | otherwise = first p tail xs
  // LAZY:
  // first p xs = if null ys then Nothing
  // else Just (head ys)
  // where ys = filter p xs

  // Ex F.
  // 1) n multiplications
  // 2) log(n) ???
}
