package io.terrafino.fp.errorhandling

import scala.{ Option => _, Some => _, Either => _, _ } // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None    => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None    => default
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f(_)) getOrElse None
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil    => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(a => a)

  // val ofh: Option[B] = f(h)
  // val x: Option[List[B]] = ofh flatMap ((b: B) => traverse(t)(f))
  // val res = x map ((l: List[B]) => ofh :: l)
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil    => Some(Nil)
    case h :: t => f(h) flatMap (fhh => traverse(t)(f) map (fhh :: _))
  }

  def Try[A](a: => A): Option[A] =
    try { Some(a) }
    catch { case e: Exception => None }

}

