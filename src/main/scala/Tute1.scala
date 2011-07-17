//import java.util.HashMap
//import java.util.Map

object Test {

  def apply(string: String) = {
    string
  }

  def main(args: Array[String]) {
    //println("Hello World")
    //println(Test("hellow"))
    val list = List("Johnny", "Charles", "Matt", "Michael", "Rob")
    println(list.map((string: String) => string.toUpperCase()))
    println(list.map(_.toUpperCase()))
    /**
     * list.map(new Callback<String>() {
     * public String callback(String arg) {
     * return arg.toUpperCase();
     * }
     * })
     */
    //println(list.foldLeft(0)((a:Int, b:String) => a + b.length))

    val someNum: Int = 4
    println(someNum + 5)

    def toUpper(string: String) = string.toUpperCase()
    println(list.map(toUpper))

    //	var hashMap:Map[String, String] = new HashMap();
    //	hashMap.put("a", "b")
    //	println(hashMap)
    /*
	
	
	val map = Map("a" -> "b", "c" -> "d")
   // println(list)
    
    var map2 = Map("a" -> "b", "c" -> "d")
    map2 = map2 + "e".->("f")
	//println(map2)
	takeMap(map2)
	
	def takeMap(someMap:Map[String, String]) = someMap
*/

    case class JavaBean(val x: String, val y: String)

    println(JavaBean("1", "2"))
    val bean = JavaBean("1", "2")
    bean match {
      case JavaBean("1", "2") => println("This one")
      case JavaBean("2", "1") => println("NO")
      case _ => println("Nothing")
    }
    "hello" match {
      case "hello" => println("This one")
      case _ => println("Nothing")
    }
    
    val tern = if (1 == 2) "false" else "true"
      println(tern)
      
      
    /*
     * if (a != null) {
     * if (b != null) {
     * etc...
     * a + b + c
     */
    def returnOption(string:String):Option[String] = {
      if (string == null) None else Some(string)
    }
    if (returnOption("a").isDefined) {
      if (returnOption(null).isDefined) {
        println("In here")
      }
    }
    val string:Option[Option[String]] = None
    println(string.flatMap(_.map(_.toUpperCase)))
    val monad:Option[String] = for {
      a <- string
      b <- a
    } yield b.toUpperCase()
    println(monad)
    
    for(i <- 1 until 10) {
      println(i)
    }
    
    trait Hello {
      def helloWorld():String
      def helloWorldImpl = helloWorld + " again"
    }
    
    trait Hello2  {
      def helloWorld():String
      def helloWorldImpl = helloWorld + " again"
    }
    
    
    class HelloImpl extends Hello with Hello2 {
    	override def helloWorld = "Hello"
    }
    println(new HelloImpl().helloWorldImpl)
    println(new HelloImpl().helloWorldImpl)
    
 //   returnOption("a").flatMa
  }

  /**
   * public String returnString() {
   * return "hello";
   * }
   */
  def returnString = {
    "hello"
    println("void")
  }
}