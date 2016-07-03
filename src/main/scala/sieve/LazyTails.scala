package sieve

object LazyTails {

  def primes = 2 #:: minus(Stream.from(3), composites)

  def composites: Stream[Int] =
    union(for (p <- primes) yield multiples(p))

  def multiples(n: Int) = Stream.from(n) map (n * _)

  def minus(s: Stream[Int], t: Stream[Int]): Stream[Int] = {
    // Simulate lazy matching on stream tails
    val (x, y) = (s.head, t.head)
    val (xs, ys) = (() => s.tail, () => t.tail)
    if      (x <  y)   x #:: minus(xs(), y #:: ys())
    else if (x == y)   minus(xs(), ys())
    else  /* x >  y */ minus(x #:: xs(), ys())
  }

  def union(ss: Stream[Stream[Int]]): Stream[Int] =
    // Use lazy fold
    lazyFoldRight(Stream[Int]())(merge)(ss)

  def lazyFoldRight[A, B](z: B)(op: (A, => B) => B)(xs: Stream[A]): B =
    if (xs.isEmpty) z
    else op(xs.head, lazyFoldRight(z)(op)(xs.tail))

  def merge(s: Stream[Int], t: => Stream[Int]): Stream[Int] = {
    // Simulate lazy matching on stream tails
    val (x, xs) = (s.head, () => s.tail)
    x #:: merge1(xs(), t)
  }

  def merge1(s: Stream[Int], t: Stream[Int]): Stream[Int] = {
    // Simulate lazy matching on stream tails
    val (x, y) = (s.head, t.head)
    val (xs, ys) = (() => s.tail, () => t.tail)
    if      (x <  y)   x #:: merge1(xs(), y #:: ys())
    else if (x == y)   x #:: merge1(xs(), ys())
    else  /* x >  y */ y #:: merge1(x #:: xs(), ys())
  }

  def main(args: Array[String]): Unit = {
    try
      println((primes take 20).toList)
    catch {
      case ex: StackOverflowError => println("Stack Overflow")
    }
  }
}
