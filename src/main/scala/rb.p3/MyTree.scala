package rb.p3

import scala.annotation.tailrec

enum Tree[+A] {
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) =>
      1 + l.size + r.size

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

  def fold[B](acc: B)(lf: A => B)(bf: (Tree[A], Tree[A]) => B): B = ???

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
  }
}

object MyTree extends App {}
