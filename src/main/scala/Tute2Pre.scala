import scala.runtime.RichInt
object List2 {

  import ListEx._
  import ListInt._

  import Monoid._
  import Comparable._

  def main(args: Array[String]) {
    """Some multiline string good for "quoted" strings"""

    val EmailParser = """([\w\d\-\_]+)(\+\d+)?@([\w\d\-\.]+)""".r
    val s = "zippy@scalaisgreat.com"
    val EmailParser(name, _, domain) = s
    printf("Name: %s, Domain: %s\n", name, domain)

    // Tuples
    val (a, b) = ("a", 1)
    val (x, y, z) = ("x", 2, "z")

    // Default params
    def defaultParams(a: String = "", b: Int = 0) = println(a, b)
    defaultParams()
    defaultParams("a", 0)
    defaultParams(b = 1)
    defaultParams(a = "b")

    val l = List(1, 2)
    val l2 = List("a", "b")
    println(l + " " + l2)
    println(sum(l))
    println(length(l))
    println(append(l2))
    println(l.foldRight(0)((x, xs) => x + xs))
    println(l.foldRight(0)(_ + _))

    // Pimped int
    println(new RichInt(1).until(10))
    println(1 until 10)
    //println(new StringOps("abc %s").format("def"))
    println("abc %s".format("def"))
    //implicit def stringWrapper(x: String) = new runtime.RichString(x)


    println(l.length())
    println(l.sum())
    //println(l2.sum())

    println("Max", l.max)
    println("Max", l2.min)

    println("Implicit int monoid", l.mappend)
    println("Implicit string monoid", l2.mappend)
    //val ll: List[List[String]] = Cons(Cons("a", Nil), Cons(Cons("b", Nil), Nil))
    val ll: List[List[String]] = List(List("a", "b"), List("c", "d"))
    println("Implicit list monoid", ll.mappend)
    val lt = List(("a", 1), ("b", 2), ("c", 3))
    println("Implicit tuple monoid", lt.mappend)

    //val tree = Node(Node(Leaf("a"), Leaf("b")), Leaf("c"))
    val tree = Node("a", Node("b"), Node("c"))
    println("Monoid String Tree", tree.flatten)
    val tree2 = Node(1, Node(2), Node(3))
    println("Monoid Int Tree", tree2.flatten)
  }

  def sum(list: List[Int]): Int = list match {
    case Cons(head, tail) => head + sum(tail)
    case Nil => 0
  }

  def append(list: List[String]): String = list match {
    case Cons(head, tail) => head + append(tail)
    case Nil => ""
  }

  def length[A](list: List[A]): Int = list match {
    case Cons(head, tail) => 1 + length(tail)
    case Nil => 0
  }
}

sealed abstract class List[+T] {
  def foldRight[A](empty: A)(f: (T, A) => A): A = this match {
    case Cons(head, tail) => f(head, tail.foldRight(empty)(f))
    case Nil => empty
  }
  override def toString = "[" + foldRight("")((a: T, b: String) => a + (if (b.isEmpty) "" else "," + b)) + "]"
}
case class Cons[T](val head: T, val tail: List[T]) extends List[T]
case object Nil extends List[Nothing]

object List {
  def apply[T](xs: T*): List[T] = xs.foldRight(Nil: List[T])((a: T, b: List[T]) => Cons(a, b))
}

object ListEx {

  implicit def pimpedList[T](list: List[T]): ListEx[T] = new ListEx(list)
}

class ListEx[T](val list: List[T]) {
  import ListEx._
  /*
  def sum(): Int = list match {
    case Cons(head: Int, tail: List[Int]) => head + tail.sum()
    case Nil => 0
    case _ => throw new RuntimeException("Not supported")
  }
*/
  def append(): String = list match {
    case Cons(head, tail) => head + tail.append()
    case Nil => ""
  }

  def length(): Int = list match {
    case Cons(head, tail) => 1 + tail.length()
    case Nil => 0
  }

  def mappend[A >: T](implicit monoid: Monoid[A]): A = {
    list.foldRight(monoid.identity)(monoid.append)
  }

  def max[A >: T](implicit comp: Comparable[A]): Option[T] = {
    list.foldRight(None: Option[T])((a: T, b: Option[T]) => if (b.isEmpty || comp.compare(a, b.get) > 0) Some(a) else b)
  }
  def min[A >: T](implicit comp: Comparable[A]): Option[T] = {
    list.foldRight(None: Option[T])((a: T, b: Option[T]) => if (b.isEmpty || comp.compare(a, b.get) < 0) Some(a) else b)
  }
}

object ListInt {

  implicit def pimpedList2(list: List[Int]): ListInt = new ListInt(list)
}

class ListInt(val list: List[Int]) {
  import ListInt._

  def sum(): Int = list match {
    case Cons(head, tail) => head + tail.sum()
    case Nil => 0
  }
}

trait Comparable[T] {
  def compare(a: T, b: T): Int
}

object Comparable {
  implicit def intComp: Comparable[Int] = new Comparable[Int] {
    override def compare(a: Int, b: Int) = b - a
  }
  implicit def stringComp: Comparable[String] = new Comparable[String] {
    override def compare(a: String, b: String) = a.compareTo(b)
  }
}

trait Monoid[A] {
  val identity: A
  def append(a: A, b: A): A
}

object Monoid {
  implicit def sumMonoid: Monoid[Int] = new SumMonoid()
  implicit def strMonoid: Monoid[String] = new StringMonoid()
  implicit def listMonoid[T]: Monoid[List[T]] = new ListMonoid[T]()
  implicit def tupleMonoid[A, B](implicit a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override val identity = (a.identity, b.identity)
    override def append(aa: Tuple2[A, B], bb: Tuple2[A, B]) = (a.append(aa._1, bb._1), b.append(aa._2, bb._2))
  }
}

class SumMonoid extends Monoid[Int] {

  override val identity = 0
  override def append(a: Int, b: Int) = a + b
}

class StringMonoid extends Monoid[String] {

  override val identity = ""
  override def append(a: String, b: String) = a + b
}

class ListMonoid[T] extends Monoid[List[T]] {

  override val identity = Nil
  override def append(a: List[T], b: List[T]): List[T] = a match {
    case Nil => b
    case Cons(head, tail) => Cons(head, append(tail, b))
  }
}

abstract class BTree[+T] {
  def fold[A](leaf: A, node: (T, A, A) => A): A = this match {
    case Empty => leaf
    case Node(elem, left, right) => node(elem, left.fold(leaf, node), right.fold(leaf, node))
  }
  def flatten[A >: T](implicit m: Monoid[A]) = fold(m.identity, (a: T, b: A, c: A) => m.append(a, m.append(b, c)))
}
case class Node[T](val elem: T, val left: BTree[T] = Empty, val right: BTree[T] = Empty) extends BTree[T]
case object Empty extends BTree[Nothing]

