package sieve

object Working {

  val primes = 2 #:: minus(Stream.from(3), composites)

  def composites: Stream[Int] =
    union(for (p <- primes) yield multiples(p))

  def multiples(n: Int) = Stream.from(n) map (n * _)

  def minus(xs: Stream[Int], ys: Stream[Int]): Stream[Int] =
    // Avoid pattern matching (at least on stream tails)
    if      (xs.head <  ys.head)   xs.head #:: minus(xs.tail, ys)
    else if (xs.head == ys.head)   minus(xs.tail, ys.tail)
    else  /* xs.head >  ys.head */ minus(xs, ys.tail)

  def union(xss: Stream[Stream[Int]]): Stream[Int] =
    // Use a lazier version of the foldRight method
    lazyFoldRight(merge, Stream[Int]())(xss)

  def lazyFoldRight[A, B](combine: (A, => B) => B, base: B)(xs: Stream[A]): B =
    // Avoid pattern matching, at least on stream tails
    if (xs.isEmpty)
      base
    else
      combine(xs.head, lazyFoldRight(combine, base)(xs.tail))

  def merge(xs: Stream[Int], ys: => Stream[Int]): Stream[Int] = {
    // Avoid pattern matching (at least on stream tails)
    def merge1(xs: Stream[Int], ys: Stream[Int]): Stream[Int] =
      if      (xs.head < ys.head)    xs.head #:: merge1(xs.tail, ys)
      else if (xs.head == ys.head)   xs.head #:: merge1(xs.tail, ys.tail)
      else  /* xs.head >  ys.head */ ys.head #:: merge1(xs, ys.tail)

    xs.head #:: merge1(xs.tail, ys)
  }

  def main(args: Array[String]): Unit = {
    try
      println((primes take 20).toList)
    catch {
      case ex: StackOverflowError => println("Stack Overflow")
    }
  }
}
