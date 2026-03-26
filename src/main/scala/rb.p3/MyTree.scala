package rb.p3

import scala.annotation.tailrec

enum Tree[+A] {
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) =>
      1 + l.size + r.size

  def sizeViaFold: Int = fold(_ => 1)((l, r) => 1 + l + r)

  def depthViaFold: Int = fold(_ => 0)((l, r) => 1 + (l max r))

  def mapViaFold[B](f: A => B): Tree[B] = fold(v => Leaf(f(v)))(Branch(_, _))

  /* alternative version:
    def depth: Int = this match
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (l.depth max r.depth)
   */
  def depth: Int = {
    def go(tree: Tree[A], depth: Int): Int = tree match
      case Leaf(_)             => depth
      case Branch(left, right) => {
        val leftDepth  = go(left, depth + 1)
        val rightDepth = go(right, depth + 1)

        leftDepth max rightDepth
      }

    go(this, 0)
  }

  def fold[B](lf: A => B)(bf: (B, B) => B): B = this match
    case Leaf(value)  => lf(value)
    case Branch(l, r) => bf(l.fold(lf)(bf), r.fold(lf)(bf))

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
}

object syntax {
  import Tree.*
  extension (t: Tree[Int]) {
    def firstPositive: Int = t match
      case Leaf(v)      => v
      case Branch(l, r) =>
        val lpos = l.firstPositive
        if lpos > 0 then lpos else r.firstPositive

    def maximum: Int = t match
      case Leaf(v)      => v
      case Branch(l, r) => l.maximum max r.maximum

    def maximumViaFold: Int = t.fold(identity)(_ max _)
  }
}

object MyTree extends App {}
