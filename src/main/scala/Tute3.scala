package

object Test2 {
  def mainx(args: Array[String]) {

    def partially(a: Int, b: Int): Int = a + b
    val part1: Function1[Int, Int] = partially(1, _: Int)
    println(part1(2))

    def partially2(a: Int)(b: Int): Int = a + b
    val part2 = partially2(1) _
    println(part2(2))

    def partially3(): ((Int, Int) => Int) = (a, b) => a + b
    println(partially3()(1, 2))

    def partially4(): (Int => Int => Int) = (a) => (b) => a + b
    println(partially4()(1)(2))

    val curried = (partially _).curried
    println(curried(1)(2))
    val uncurried = Function.uncurried(curried)
    println(uncurried(1, 2))


    import OptionFunctor._

    println(fmap((a: String) => a.length())(Some("abc")))
  }

  implicit def optionFunctor: Functor[Option] = OptionFunctor
}

trait Functor[M[_]] {
  def fmap[A, B](f: A => B): M[A] => M[B]
}

object OptionFunctor extends Functor[Option] {
  override def fmap[A, B](f: A => B) = {
    case Some(a) => Some(f(a))
    case None => None
  }
}