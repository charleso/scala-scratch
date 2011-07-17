
// http://blog.tmorris.net/debut-with-a-catamorphism/
trait MyOption[+A] {

  import MyOption._
 
  // single abstract method
  def cata[X](some: A => X, none: => X): X
 
  def map[B](f: A => B): MyOption[B] = cata(a => some(f(a)), none)
 
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = cata(f, none)
 
  def getOrElse[AA >: A](e: => AA): AA = cata(a => a, e)
 
  def filter(p: A => Boolean): MyOption[A] = cata(a => if (p(a)) some(a) else none, none)
 
  def foreach(f: A => Unit): Unit = cata(a => f(a), ())
 
  def isDefined: Boolean = cata(_ => true, false)
 
  def isEmpty: Boolean = !isDefined
 
  // WARNING: not defined for None
  def get: A = cata(a => a, error("not defined"))
 
  def orElse[AA >: A](o: MyOption[AA]): MyOption[AA] = cata(some(_), o)
 
  def toLeft[X](right: => X): Either[A, X] = cata(a => Left(a), Right(right))
 
  def toRight[X](left: => X): Either[X, A] = cata(a => Right(a), Left(left))
 
  def toList: List[A] = cata(List(_), Nil)
 
  def iterator: Iterator[A] = toList.iterator
}
 
object MyOption {
  def none[A] = new MyOption[A] {
    def cata[X](s: A => X, n: => X) = n
  }
 
  def some[A](a: A) = new MyOption[A] {
    def cata[X](s: A => X, n: => X) = s(a)
  }
}