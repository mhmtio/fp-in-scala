package io.terrafino.fp.datastructures

object Fibonacci extends App {

  def fib1(n: Int): Int = {
    if (n == 0 || n == 1) n
    else fib1(n - 1) + fib1(n - 2)
  }

  def fib2(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0) prev
      else go(n - 1, curr, prev + curr)
    }
    go(n, 0, 1)
  }

  for (i <- 0 to 10) {
    println(s"$i : ${fib1(i)} : ${fib2(i)}")
  }
}