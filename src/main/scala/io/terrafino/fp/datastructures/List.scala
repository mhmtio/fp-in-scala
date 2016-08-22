package io.terrafino.fp.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil         => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil         => throw new IllegalAccessException("Nil does not have tail!")
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _)         => Nil
    case (l, 0)           => l
    case (Cons(x, xs), n) => drop(xs, n - 1)
  }

  def take[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _)         => Nil
    case (l, 0)           => Nil
    case (Cons(x, xs), n) => Cons(x, take(xs, n - 1))
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
  
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((x, y) => y+1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum2(l: List[Int]): Int = 
    foldLeft(l, 0)(_ + _)
  
  def product2(l: List[Int]): Int = 
    foldLeft(l, 0)(_ * _)
  
  def length2(l: List[Int]): Int = 
    foldLeft(l, 0)((x, y) => x + 1)

  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, Nil:List[A])((x, y) => Cons(y, x))

  def append[A](l1: List[A], l2: List[A]): List[A] = 
    foldRight(l1, l2)((x, y) => Cons(x, y))

  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())((x, y) => append(x, y))

  def flatten2[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A]())((x, y) => append(x, y))
    
  def map[A, B](as: List[A])(f: A => B): List[B] = 
    foldRight(as, Nil:List[B])((x:A, y:List[B]) => Cons(f(x), y))
    
  def filter[A](as: List[A])(f: A => Boolean): List[A] = 
    foldRight(as, Nil:List[A])((x, y) => if (f(x)) Cons(x, y) else y)
    
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))
    
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = 
    flatMap(as)(x => if (f(x)) List(x) else List())
    
  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => (Cons(f(x, y), zipWith(xs, ys)(f)))
  }
  
  def hasSubSequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => false
    case Cons(x, xs) => if (take(l, length(sub)) == sub) true else hasSubSequence(xs, sub)
  }
    
}