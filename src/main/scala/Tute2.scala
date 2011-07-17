import scala.runtime.RichInt

import ListEx._

import Comparable._

object Tut2 {

  def HELP() = "".codePointCount _
  
  def main(args: Array[String]) {

    // Default params
    /*
     * public void test() {
     * 	test(0);
     * }
     * 
     * public void test(int i) {
     * test("", i)
     * }
     * 
     * pulic void test(String s, int i) {
     * }
     */
    def defaultParams(a: String = "", b: Int = 0) = println(a, b)
    defaultParams()
    defaultParams("a", 0)
    defaultParams(b = 1)
    defaultParams(a = "b")

    
    
    
    // Tuples
    def returnTuple():Tuple5[Int, Int, String, Int, String] = (0, 1, "", 0, "")
    
    val (a, b) = ("a", 1)
    val xxx = ("a", 1)
    println(xxx._1)
    println(xxx._2)
    println(b.+(0))
    val (x, y, z) = ("x", 2, "z")

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    """Some multiline string good for "quoted" strings"""

    
    
    
    val EmailParser = """([\w\d\-\_]+)(\+\d+)?@([\w\d\-\.]+)""".r
    val s = "zippy@scalaisgreat.com"
    val EmailParser(name, _, domain) = s
    printf("Name: %s, Domain: %s\n", name, domain)

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    // Pimped int
    println(new RichInt(1).until(10))
    for(iii <- 1.until(10)) {
      
    }
    println(1 until 10)
    //println(new StringOps("abc %s").format("def"))
    println("abc %s".format("def"))
    //implicit def stringWrapper(x: String) = new runtime.RichString(x)


    val l = List(1, 2, 3, 4, 5)
    val l2 = List("a", "b")
    val l3 = List(("a", "b"), ("x", "y"))

    println("Length", new ListEx(l).length2)
    println("Length", l.length2)

    
    
    /*
     * public interface Comparable<T> {
     * 	int compare(T a, T b)
     * }
     * Collections.sort(list, new COmparable())
     * 
     * list<T>.sort(new Comparable<T>())
     */
    
    
    
    
    
    
    
    
    
    
    
    import Equals._
    
    println("ISEQUALS", isActualEquals("a", "b")(stringEquals))
    println("ISEQUALS", isActualEquals("a", "b")(stringEquals2))
     //println("ISEQUALS", isActualEquals(1, 2))
    
    println("Max", l.max2)
 //   println("Min", new ListEx(l2).min2(intComp()))
   
    // sort :: Comparable a => [a] -> [a]
    
 //   println("Min", l3.min2)
  }
}

object ListEx {

  implicit def pimpedList[T](list: List[T]): ListEx[T] = new ListEx(list)
}

class ListEx[T](val list: List[T]) {
  import ListEx._

  def length2(): Int = list.foldRight(0)((_, b: Int) => 1 + b)

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  def max2[A >: T](implicit comp: Comparable[A]): Option[T] = {
    list.foldRight(None: Option[T])((a: T, b: Option[T]) => if (b.isEmpty || comp.compare(a, b.get) > 0) Some(a) else b)
  }
  def min2[A >: T](implicit comp: Comparable[A]): Option[T] = {
    list.foldRight(None: Option[T])((a: T, b: Option[T]) => if (b.isEmpty || comp.compare(a, b.get) < 0) Some(a) else b)
  }
}

trait Equals[T] {
  def isEquals(a: T, b: T): Boolean
}

object Equals {
  implicit def stringEquals: Equals[String] = new Equals[String] {
    override def isEquals(a:String, b:String): Boolean = {
      a == b
    }
  }
   implicit def stringEquals2: Equals[String] = new Equals[String] {
    override def isEquals(a:String, b:String): Boolean = {
      a != b
    }
  }
  
  def isActualEquals[T](a:T, b:T)(implicit e:Equals[T]) = e.isEquals(a, b)
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
