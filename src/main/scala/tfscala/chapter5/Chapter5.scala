package tfscala.chapter5

object Chap5Module {
  def main(args: Array[String]): Unit = {
    import Sudoku._

    println(s"Digits: ${digits}")
    println(s"Blank[ 0: ${blank('0')}, 2: ${blank('2')}]")

    val grid000: Grid = Matrix(
      Row('4', '6', '0', '3', '0', '0', '0', '0', '8'),
      Row('0', '0', '0', '4', '8', '9', '0', '0', '3'),
      Row('3', '0', '8', '0', '2', '5', '4', '0', '0'),
      Row('8', '0', '2', '0', '5', '0', '6', '0', '9'),
      Row('9', '0', '0', '0', '0', '0', '0', '0', '5'),
      Row('1', '0', '6', '0', '9', '0', '7', '0', '2'),
      Row('0', '0', '3', '2', '1', '0', '9', '0', '4'),
      Row('5', '0', '0', '9', '3', '6', '0', '0', '0'),
      Row('7', '0', '0', '0', '0', '8', '0', '1', '6'))

    val grid001: Grid = Matrix(
      Row('9', '5', '4', '0', '8', '0', '6', '1', '3'),
      Row('6', '3', '8', '9', '1', '5', '7', '4', '2'),
      Row('7', '1', '2', '3', '4', '6', '8', '9', '5'),
      Row('3', '9', '5', '1', '6', '7', '4', '2', '8'),
      Row('1', '4', '6', '8', '2', '3', '9', '5', '7'),
      Row('8', '2', '7', '4', '5', '9', '1', '3', '6'),
      Row('4', '7', '1', '5', '3', '8', '2', '6', '9'),
      Row('2', '8', '3', '6', '9', '1', '5', '7', '4'),
      Row('5', '6', '9', '2', '7', '4', '3', '8', '1'))

    println(s"Choices 000: ${choices(grid000)}")

    println(s"cp [[1], [2], [3]]: ${cp(List(List(1), List(2), List(3)))}")
    println(s"cp [[1, 2], [], [4,5]]: ${cp(List(List(1,2), List(), List(4,5)))}")
    println(s"cp [[1, 2], [3], [4,5]]: ${cp(List(List(1,2), List(3), List(4,5)))}")

    val smallGrid: Grid = Matrix(
      Row('0', '1', '2'),
      Row('3', '4', '5'),
      Row('6', '7', '8'))

    println(s"cols smallGrid: ${cols(smallGrid)}")
    println(s"boxs 000: ${boxs(grid000)}")

    // println(s"Solve 000: ${solve(grid000)}")
    println(s"Solve 001: ${solve(grid001)}")
  }
}

object Sudoku {
  type Matrix[A] = List[Row[A]]
  def Matrix[A](xs: Row[A]*) = List(xs: _*)

  type Row[A] = List[A]
  def Row[A](xs: A*) = List(xs: _*)

  type Grid = Matrix[Digit]
  type Digit = Char

  def digits: List[Digit] = List.range('1', ':')
  def blank(d: Digit): Boolean = d == '0'

  def solve: Grid => List[Grid] = filter(valid) _ compose completions

  def filter[A](p: (A) ⇒ Boolean)(xs: List[A]): List[A] = xs.filter(p)

  def completions: Grid => List[Grid] = expand _ compose choices

  def valid(g: Grid): Boolean = all(nodups)(rows(g)) &&
    all(nodups)(cols(g)) &&
    all(nodups)(boxs(g))

  // TODO: in haskell map (map choices) which reads much better - explore a little bit
  def choices(g: Grid): Matrix[List[Digit]] = map{r:Row[Digit] => map(choice)(r)}(g)

  def map[A, B](f: (A) ⇒ B)(xs: List[A]): List[B] = xs.map(f)

  def choice(d: Digit): List[Digit] = if (blank(d)) digits else List(d)

  // TODO: in haskell cp (map cp)
  def expand(choices: Matrix[List[Digit]]): List[Grid] =  cp (map(cp[Digit])(choices))

  def cp[A](m: List[List[A]]): List[List[A]] = m match {
    case xs::xss => for {
      x <- xs
      ys <- cp(xss)
    } yield x::ys
    case Nil => List(List())
  }

  def all[A](p: (A) ⇒ Boolean)(xs: List[A]): Boolean = xs.forall(p)

  def nodups[A](xs: List[A]): Boolean = xs match {
    case Nil => true
    case x :: xs => all{a: A => a != x }(xs) && nodups(xs)
  }

  def rows[A](m: Matrix[A]): Matrix[A] = m
  def cols[A](m: Matrix[A]): Matrix[A] = m match {
    case List(xs) => for {
      x <- xs
    } yield List(x)
    case xs::xss => zipWith((y: A, ys: Row[A]) => y :: ys)(xs)(cols(xss))
  }

  def boxs[A]: Matrix[A] => Matrix[A] = map(ungroup[A]) _ compose ungroup[List[List[A]]] compose map(cols[List[A]]) compose group[List[List[A]]] compose map(group[A])

  def zipWith[A, B, C](f: (A, B) => C)(xs: List[A])(ys: List[B]): List[C] = (xs, ys).zipped map (f)

  def group[A](xs: List[A]): List[List[A]] = xs match {
    case Nil => List()
    case xs => xs.take(3) :: group (xs.drop(3))
  }

  def ungroup[A](xss: List[List[A]]): List[A] = xss.flatten

  val test4by4:Grid = Matrix(
    Row('a', 'b', 'c', 'd'),
    Row('e', 'f', 'g', 'h'),
    Row('i', 'j', 'k', 'l'),
    Row('m', 'n', 'o', 'p'))
}

/*


[['a', 'b', 'c', 'd'],
 ['e', 'f', 'g', 'h'],
 ['i', 'j', 'k', 'l'],
 ['m', 'n', 'o', 'p'],


map(group[Char])(test4by4)

[[[a, b], [c, d]],
 [[e, f], [g, h]],
 [[i, j], [k, l]],
 [[m, n], [o, p]]]

scala> val f000 = group[List[List[Char]]] _ compose map(group[Char])
f000: List[List[Char]] => List[List[List[List[Char]]]] = <function1>

scala> f000(test4by4)
res2: List[List[List[List[Char]]]] = List(List(List(List(a, b), List(c, d)), List(List(e, f), List(g, h))), List(List(List(i, j), List(k, l)), List(List(m, n), List(o, p))))



((((a, b), (c, d)),((e, f), (g, h))),
 (((i, j), (k, l)),((m, n), (o, p))))


scala> val f001 = map(cols[List[Char]]) _ compose group[List[List[Char]]] compose map(group[Char])
f001: List[List[Char]] => List[tfscala.chapter5.Sudoku.Matrix[List[Char]]] = <function1>

scala> f001(test4by4)
res3: List[tfscala.chapter5.Sudoku.Matrix[List[Char]]] = List(List(List(List(a, b), List(e, f)), List(List(c, d), List(g, h))), List(List(List(i, j), List(m, n)), List(List(k, l), List(o, p))))



 */
