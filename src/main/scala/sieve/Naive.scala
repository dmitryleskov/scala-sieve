package sieve

object Naive {

  def primes = 2 #:: minus(Stream.from(3), composites)

  def composites: Stream[Int] =
    union(for (p <- primes) yield multiples(p))

  def multiples(n: Int) = Stream.from(n) map (n * _)

  val minus: (Stream[Int], Stream[Int]) => Stream[Int] = {
    case (x#::xs, y#::ys) =>
      if      (x <  y)   x #:: minus(xs, y#::ys)
      else if (x == y)   minus(xs, ys)
      else  /* x >  y */ minus(x#::xs, ys)
  }

  def union(ss: Stream[Stream[Int]]): Stream[Int] =
    ss.foldRight(Stream[Int]())(merge)

  val merge: (Stream[Int], Stream[Int]) => Stream[Int] = {
    case (x#::xs, ys) => x #:: merge1(xs, ys)
  }

  val merge1: (Stream[Int], Stream[Int]) => Stream[Int] = {
    case (x#::xs, y#::ys) =>
      if      (x <  y)   x #:: merge1(xs, y#::ys)
      else if (x == y)   x #:: merge1(xs, ys)
      else  /* x >  y */ y #:: merge1(x#::xs, ys)
  }

  def main(args: Array[String]): Unit = {
    try
      println((primes take 20).toList)
    catch {
      case ex: StackOverflowError => println("Stack Overflow")
    }
  }
}
