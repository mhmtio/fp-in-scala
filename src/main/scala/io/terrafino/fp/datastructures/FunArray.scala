package io.terrafino.fp.datastructures

object FunArray extends App {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
     if (n+1 >= as.size) true
     else if (!ordered(as(n), as(n+1))) false
     else loop(n+1)
    }
    loop(0)
  }
  
  val res = isSorted(Array(1,2,3,4,1), (a: Int, b: Int) => a <= b)
  println(res)
}