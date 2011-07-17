
/*
Scalable Language
Started in 2001.
LinkedIn. Guardian. Twitter. 
*/

object Main {
  // Note: no return type
  // Must have generics
  def main(args: Array[String]) {
    // Note the lack of semicolons
    println("Hello world")
    println("""Multi
line
String
""")

    val l = List("Johnny", "Charles", "Stephen", "Matt", "Michael", "Rob")
    // Map<String, Map<String, String>> blah = new HashMap<String, Map<String, String>>();
    val shortMap = Map("a" -> Map("b" -> "c"))
    println(l.map((s:String) => s.toUpperCase))
    println(l.map(_.toUpperCase))
    println(l.filter(_.length < 5))
    println(l.reverse)
    println(l.sortBy(_.charAt(0)))
    println(l.sort(_ < _))
    println(l.slice(2, 3))

    val m = Map("a" -> "b")
    println(l)
    println(m)
    m + ("x" -> "y")
    println("Immutable map " + m)

    import collection.mutable.Map
    val m2 = Map("a" -> "b")
    m2 += ("x" -> "y")
    println("Mutable map: " + m2)

    // TODO Everything is an object 
    println("Everything is an object: " + (1 + 2))
    println("Everything is an object: " + (1. + (2)))

    // No dot
    println("something".length())
    println("something" length ())
    println("something" length)
    println("something" contains "some")

    // TODO Inner methods
    def innerMethod(i: Int) = i + 1
    println("inner method" + innerMethod(1))

    // TODO Closure
    def closure(c: (String, String) => Int) = c("a", "b")
    println("Closure test", closure((a, b) => (a + b).length))

    def lazyvalue(l: => String) = "value"
    println("Lazy", lazyvalue("not eval"))
    println("Lazy", lazyvalue({ println("Not evaluated"); "not eval" }))

    // Quicksort
    def qsort: List[Int] => List[Int] = {
      case Nil => Nil
      case pivot :: tail =>
        val (smaller, rest) = tail.partition(_ < pivot)
        qsort(smaller) ::: pivot :: qsort(rest)
    }
    println("QSorxt", qsort(List(5, 7, 3, 2, 9, 1)))

    // TODO Trait
    trait Comparable[A] {
      def compare(that: A): Int

      def <(that: A): Boolean = (this compare that) < 0
      def >(that: A): Boolean = (this compare that) > 0
      def <=(that: A): Boolean = (this compare that) <= 0
      def >=(that: A): Boolean = (this compare that) >= 0
      def compareTo(that: A): Int = compare(that)
    }
    case class Num(val i: Int) extends Comparable[Num] {
      override def compare(that: Num) = this.i compare that.i
    }
    println("Compare 9<10", Num(9) < Num(10))

    // TODO Get/set
    class NoGetSet(var value: String = "")
    class NoGetSet2 {
      private var _value = ""
      def value = _value + "1"
      def value_=(_v: String) { _value = _v }
    }
    val getset = new NoGetSet();
    getset.value = "a"
    println("get set", getset.value)

    // TODO Case classes
    case class Position(val x: Int, val y: Int)

    val b1 = new Position(1, 2)
    val b2 = Position(1, 2)
    val b = new Position(y = 1, x = 2)
    b match {
      case Position(2, 1) => println("This one")
      case Position(1, 2) => println("That one")
      case _ => println("Not found")
    }
    List(1, 2, 3) match {
      case List(2, 3, 4) => println("Not this one")
      case List(1, 2, 3) => println("This one?")
      case _ => println("Not found")
    }

    // TODO Objects vs Classes
    class MyClass(val value: String)
    object MyClass {
      def apply(value: String) = new MyClass(value)
    }
    println(new MyClass("hello"))
    println(MyClass("hello"))

    // TODO Everthing is an expression
    val expression = if (1 == 2) "true" else "false"
    println(expression)
    val anotherExpression = for (i <- 1 until 10)
      yield (i * i)
    println("Another expression " + anotherExpression)
    val unitExpression = for (i <- 1 until 10) {
      i + 1
    }
    println("Unit expression", unitExpression)

    def returnNull(a:String):Option[String] = if (a == null) None else Some(a)
      
    val couldBeNull = for {
      a <- returnNull(null)
      b <- returnNull(a)
      c <- returnNull(b)
    } yield c
    println("could be null", couldBeNull);
    
    // Option / Monad
    def run(option: Option[Option[String]]) {
      option.flatMap((o: Option[String]) => o.map((s: String) => println(s)))
      option.flatMap(o => o.map(s => println(s)))
      option.flatMap(_.map(println _))
      for {
        o <- option
        s <- o
      } {
        println(s)
      }
    }

    run(Some(Some("abc")))
    run(Some(None))

    // Parallel collections
    val ol = (1 to 100)
    println("Option list", ol.par.map(_ + 1).foldLeft(Nil: List[Int])((l, i) => if (i % 2 == 0) i :: l else l))
    //println("Option list", ol.par.map(_ + 1).foreach(println(_)))

    // Links
    // http://www.codecommit.com/blog/scala/scala-for-java-refugees-part-1
  }

}