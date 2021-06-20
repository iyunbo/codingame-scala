package org.iyunbo.coding
package reactive

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def countNodes[A](tree: Tree[A]): Int = fold(tree)(_ => 1, (l: Int, r: Int) => l + r + 1)

  def maximumn(tree: Tree[Int]): Int = fold(tree)(x => x, (l: Int, r: Int) => l max r)

  def depth[A](tree: Tree[A]): Int = fold(tree)(x => 0, (l: Int, r: Int) => (l max r) + 1)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](tree: Tree[A])(map: A => B, combine: (B, B) => B): B = {
    tree match {
      case Leaf(value) => map(value)
      case Branch(l, r) => combine(fold(l)(map, combine), fold(r)(map, combine))
    }
  }
}
