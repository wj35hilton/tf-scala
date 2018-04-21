package tfscala.chapter4

object Chap4Module {

  import Chap4Answers._
  import SnocList._
  import SplitAt._

  // TODO: like to get the main out of here for hygenic reasons
  def main(args: Array[String]): Unit = {
    println("UMMM, HEY...");
    println("distinct pairs " + distinct(10))

    println("disjoint (true) " + disjoint(List(1,2,3), List(4,5,6)))
    println("disjoint (false) " + disjoint(List(1,2,3), List(3,5,6)))

    println("disjoint2 (true) " + disjoint2(List(1,2,3), List(4,5,6)))
    println("disjoint2 (false) " + disjoint2(List(1,2,3), List(3,5,6)))

    println("quads " + quads(40));

    println("snoc construct: " + Snoc(Snoc(Snoc(SNil, 1), 2), 3))
    println("snoc head: " + head(Snoc(Snoc(Snoc(SNil, 1), 2), 3)))
    println("snoc last: " + last(Snoc(Snoc(Snoc(SNil, 1), 2), 3)))
    println("snoc toList: " + toList(Snoc(Snoc(Snoc(SNil, 1), 2), 3)))
    println("snoc fromList: " + fromList(List(6,9,12)))

    println("scala split: " + (List(1,2,3,4,5).splitAt(2)))
    println("wjh split: " + splitAtWjh(2, List(1,2,3,4,5)))
  }
}


object Chap4Answers {

  // Exercise A: true or false
  // []:xs = xs ... false :)
  // xs:[] = xs .... false :)
  // xs:[] = [xs] ... true :)
  // [[]] ++ xs = xs false :)
  // [xs] ++ [] = [xs] true :)

  // []:xs = [[], xs] ... true :(
  // xs:xs = [xs, xs] true :(
  // [[]] ++ xs = [[],xs] true :(
  // [[]] ++ [xs] = [[],xs] false :(

  // BTW, why not:
  //   null = (==[])?

  // Exercise B:
  // enumerate all distinct pairs (x,y) of natural numbers
  // does allPairs = [(x, y) | x <- [0..], y <- [0..]] do the job?
  // nope ... can never exhaust y
  // better would be ...
  // [(x, d - x) | d <- [0..], x <- [0..d]]
  def distinct(n:Int): Seq[Tuple2[Int, Int]] = {
    val s = for(d <- Stream.from(0); x <- 0 to d) yield {(x, d - x)}
    s.take(n).toList
  }

  // Exercise C:
  // disjoint = any map foldl contains
  def disjoint[A](xs1: List[A], xs2: List[A]): Boolean = !xs1.map(xs2.contains(_)).contains(true)

  def disjoint2[A](xs: List[A], ys: List[A])(implicit ev: A => Ordered[A]): Boolean = (xs, ys) match {
    case (_, Nil) => true
    case (Nil, _) => true
    case (x :: xs, y :: ys) if x < y => disjoint2(xs, y :: ys)
    case (x :: xs, y :: ys) if y > x => disjoint2(x :: xs, ys)
    case _ => false
  }

  // Exercise D:
  // when do these produce equivalent results:
  // [e | x <- xs, p x, y <- ys]
  // [e | x <- xs, y <- ys, p x]

  // Exercise E:
  // take 3 [(a,b,c,d) | a <- [0..100], b <- [a..100], c <- [(a+1)..100], d <- [c..100], a^3 + b^3 == c^3 + d^3]
  def quads(n: Int) = for(
    a <- 0 to n;
    b <- a to n;
    c <- (a+1) to n;
    d <- c to n;
    if (scala.math.pow(a,3) + scala.math.pow(b,3) == scala.math.pow(c,3) + scala.math.pow(d,3)))
  yield {
    (a,b,c,d)
  }

  // Exercise F:
  // data List a = Nil | Snoc (List a) a deriving (Show)
  // let { head (Snoc Nil x) = x; head (Snoc xs x) = head xs }
  // last (Snoc xs x) = x
  // toList :: [a] -> List a
  // fromList :: List a -> [a]
  // let { toList :: [a] -> List a; toList = convert . reverse where convert [] = Nil; convert (x:xs) = Snoc (convert xs) x}
  // see object Snoc, below for scala

  // Exercise G:
  // ????

  // Exercise H:
  // let { take n [] = []; take n (x:xs) = if (n == 0) then [] else x:take (n-1) xs }
  // let { drop n [] = []; dnziprop n (x:xs) = if (n == 1) then xs else drop (n-1) xs }

  // take 0 undefined -> []
  // take undefined [] ?????

  // take n xs ++ drop n xs = xs ... valid for all int
  // take m . drop n = drop n . take (m+n) .... not valid
  // take m . take n = take (m `min` n) ... valid
  // drop m . drop n = drop (m+n) ... valid

  // splitAt n xs = (take n xs, drop n xs)
  // let { splitAt2 n [] = ([],[]); splitAt2 n (x:xs) = if (n==0) then ([],x:xs) else (x:ys, zs) where (ys, zs) = splitAt2 (n-1) xs}

  // Exercise I:
  // I agree with the following statements:
  // 3, 4, 5, 6(:()
  // I disagree with the following:
  // 1, 2, 7(:()

  // Exercise J:
  // T: map f . take n = take n . map f
  // T: map f . reverse = reverse . map f
  // F: map f . sort = sort . map f
  // T: map f . filter p = map fst . filter snd . map (fork (f,p))
  // T: filter (p . g) = map (invertg) . filter p . map g
  // T: reverse . concat = concat . reverse . map reverse
  // T: filter p concat = concat . map (filter p)

  // Exercise K:
  // unzip = fork (map fst, map snd)
  // cross (f, g) = fork (f . fst, g . snd)
  // Types of these functions?
  //
  // fork :: (a -> b, a -> c) -> a -> (b,c)
  // fork (f,g) x = (f x, g x)
  //
  // takes a list of tuples and returns a tuple of the first and last
  // unzip :: [(a,b)] -> ([a], [b])
  // unzip = fork (map fst, map snd)
  //
  // applies a function f to the first member of a tuple and g to the second
  // cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
  // cross (f,g) = fork (f . fst, g . snd)
  //
  // rules:
  //  1) cross (f,g) . fork (h,k) = fork (f . h, g . k)
  //  2) fork (f, g) . h = fork (f . h, g . h)
  //  3) fst . cross (f, g) = f . fst
  //  4) snd . cross (f, g) = g . snd
  // functor laws of map
  //  map id = id
  //  map (f . g) = map f . map g
  //
  // Prove that
  // cross (map f, map g) . unzip = unzip . map (cross (f,g))
  //
  // cross (map f, map g) . unzip = unzip . map (cross (f,g))
  // definition of unzip
  // cross (map f, map g) . fork (map fst, map snd) = unzip . map (cross (f,g))
  // rule 1 above
  // fork (map f . map fst, map g . map snd) = unzip . map (cross (f, g))
  // functor law 2
  // fork (map (f . fst), map (g . snd)) = unzip . map (cross (f, g))
  // definition of unzip
  // fork (map (f . fst), map (g . snd)) = fork (map fst, map snd) . map (cross (f, g))
  // rule 2 above
  // fork (map (f . fst), map (g . snd)) = fork (map fst . map (cross (f, g)), map snd . map (cross (f, g)))
  // functor law 2
  // fork (map (f . fst), map (g . snd)) = fork (map (fst . cross (f, g)), map (snd . cross (f, g)))
  // rule 3/4 above
  // fork (map (f . fst), map (g . snd)) = fork (map (f . fst), map (g . snd))

  // Exercise L:
  // Prove that
  // cross (f,g) . cross (h,k) = cross (f . h, g . k)
  //
  // cross (f,g) . cross (h,k) = cross (f . h, g . k)
  // ={definition of cross}
  // cross (f,g) . fork(h . fst, k . snd) = cross (f . h, g . k)
  // ={rule of cross . fork}
  // fork (f . (h . fst), g . (k . snd)) = cross (f . h, g . k)
  // ={definition of cross}
  // fork (f . (h . fst), g . (k . snd)) = fork (f . h . fst, g . k .snd)
  //
  // cross (id, id) = id
  // fork (id . fst, id . snd) = id
}

sealed trait SnocList[+A]
case object SNil extends SnocList[Nothing]
case class Snoc[+A](xs:SnocList[A], x:A) extends SnocList[A]

object SnocList {
  def head[A](l: SnocList[A]): A = l match {
    case SNil => sys.error("head of empty list")
    case Snoc(SNil, x) => x
    case Snoc(xs, _) => head(xs)
  }

  def last[A](l: SnocList[A]): A = l match {
    case SNil => sys.error("last of empty list")
    case Snoc(_, x) => x
  }

  def toList[A](l: SnocList[A]): List[A] = {
    def convert(l: SnocList[A]): List[A] = l match {
      case SNil => List.empty
      case Snoc(xs, x) => x :: convert(xs)
    }
    convert(l).reverse
  }

  def fromList[A](l: List[A]): SnocList[A] = {
    def convert(l: List[A]): SnocList[A] = l match {
      case Nil => SNil
      case (x :: xs) => Snoc(convert(xs), x)
    }
    convert(l.reverse)
  }
}

object SplitAt {
  def splitAtWjh[A](n: Integer, l: List[A]): Tuple2[List[A], List[A]] = l match {
    case Nil => (Nil, Nil)
    case x::xs if (n == 0) => (Nil, x::xs)
    case x::xs => {
      val ss = splitAtWjh(n - 1, xs)
      (x::ss._1, ss._2)
    }
  }
}
