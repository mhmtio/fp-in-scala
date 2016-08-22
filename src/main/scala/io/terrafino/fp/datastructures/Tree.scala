package io.terrafino.fp.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(a) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(a) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  
  def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = t match {
    case Leaf(a) => l(a)
    case Branch(x, y) => b(fold(x)(l)(b), fold(y)(l)(b))
  }
  
  def size2[A](t: Tree[A]): Int =
    fold(t)(l => 1)((x, y) => 1 + x + y)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(l => l)((x, y) => x max y)

  def depth2[A](t: Tree[A]): Int =
    fold(t)(l => 0)((x, y) => 1 + (x max y))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(l => Leaf(f(l)): Tree[B])((x, y) => Branch(x, y))
}