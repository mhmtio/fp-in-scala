package io.terrafino.fp.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _          => z
    }

  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 1)  => cons(h(), t().take(n - 1))
    case Cons(h, t) if (n == 1) => cons(h(), empty)
    case _                      => empty
  }

  def take2(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if (n > 1)  => Some((h(), (t(), n - 1)))
      case (Cons(h, t), n) if (n == 1) => Some((h(), (empty, 0)))
      case _                           => None
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => t().drop(n - 1)
    case _                     => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (f(h())) => cons(h(), t().takeWhile(f))
    case _                      => empty
  }

  def takeWhile2(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else empty)

  def takeWhile3(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if (f(h())) => Some((h(), t()))
      case _                      => None
    }

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h, t), Cons(g, s)) => Some((f(h(), g()), (t(), s())))
      case _                        => None
    }

  def startsWith[A](s: Stream[A]): Boolean = {
    val bs = unfold((this, s)) {
      case (Cons(h, t), Cons(h2, t2)) if (h() == h2()) => Some((true, (t(), t2())))
      case _ => Some((false, (empty, empty)))
    }
    bs.headOption.get
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def fibs(n: Int, m: Int): Stream[Int] = cons(n, fibs(m, n + m))
    fibs(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None         => empty
  }

  def ones2: Stream[Int] =
    unfold(1)(s => Some((1, 1)))

  def constant2[A](a: A): Stream[A] =
    unfold(a)(s => Some((a, a)))

  def from2(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def fibs2: Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }
}