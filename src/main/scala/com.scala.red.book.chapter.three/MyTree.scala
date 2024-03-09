enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  // def size: Int = this match
  //   case Leaf(_)      => 1
  //   case Branch(l, r) => 1 + l.size + r.size
  def size: Int =
    fold(a => 1, 1 + _ + _)

  def depth: Int =
    fold(a => 0, (d1, d2) => 1 + (d1 max d2))

  def map[B](f: A => B): Tree[B] =
    fold(a => Leaf(f(a)), Branch(_, _))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(a)      => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

object Tree {
  extension (t: Tree[Int])
    def firstPositive: Int = t match
      case Leaf(i) => i
      case Branch(l, r) =>
        val lpos = l.firstPositive
        if lpos > 0 then lpos else r.firstPositive

    def maximum: Int = t match
      case Leaf(i)      => i
      case Branch(l, r) => l.maximum.max(r.maximum)
}

// @main def gogo: Unit =
//   val tree: Tree[Int] =
//     Tree.Branch(
//       Tree.Branch(
//         Tree.Branch(
//           Tree.Leaf(3),
//           Tree.Leaf(9)
//         ),
//         Tree.Leaf(5)
//       ),
//       Tree.Branch(
//         Tree.Leaf(10),
//         Tree.Leaf(1)
//       )
//     )

// println(tree.firstPositive)
// println(tree.map(_ + 1))
// println(tree.depth)
