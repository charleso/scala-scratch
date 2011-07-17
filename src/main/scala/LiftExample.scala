package

// http://blog.tmorris.net/lifting/
object LiftExample {
  def main(args: Array[String]) {
    def f(s:String) = s.length()
    println(new OptionLift().lift1(f)(None))
    println(new OptionLift().lift1(f)(Some("a")))
    println(new ListLift().lift1(f)(List("ax", "ccb", "c")))
    println(new EitherLift().lift1(f)(Right("ax")))
    println(new EitherLift().lift1(f)(Left("ax")))
    println(new Function1Lift().lift1(f)((r:Int) => (r until (r+1)).toString)(4))
    println(new Function1Lift().ap((r:Int) => (a:String) => a + (r + 1))((r:Int) => (r.toString + 1))(4))
  }
}

class OptionLift extends LiftImpl[Option] {
  override def lift0[A] = Some(_)

  //override def ap[A, B] = fs => a => fs.flatMap(f => a.map(f(_)))
  override def ap[A, B] = fs => a => if (fs.isDefined && a.isDefined) Some(fs.get(a.get)) else None
}

class ListLift extends LiftImpl[List] {
  override def lift0[A] = List(_)

  override def ap[A, B] = fs => as => {
    val nil:List[B] = Nil
    fs.foldLeft(nil)((bs, f) => as.foldLeft(nil)((xs, x) => f(x) :: xs) ++ bs)
  }
}

class EitherLift[R] extends LiftImpl[({type λ[α] = Either[R, α]})#λ] {
  override def lift0[A] = Right(_)

  override def ap[A, B] = fs => as => if (fs.isRight && as.isRight) Right(fs.right.get(as.right.get)) else Left(as.left.get)
}

class Function1Lift[R] extends LiftImpl[({type λ[α] = R => α})#λ] {
  override def lift0[A] = a => ((_:R) => a)

  // F[A => B] => F[A] => F[B]
  //override def ap[A, B] = (fs:Function1[R, Function1[A, B]]) => (as:Function1[R, A]) => (r:R) => error("Not impl")
  override def ap[A, B] = fs => as => r => fs(r)(as(r))
}

trait Lift[F[_]] {
  // Spot the pattern in these type signatures
  // of increasing arity

  def lift0[A]:
  A => F[A]

  def lift1[A, B]:
  (A => B) => (F[A] => F[B])

  def lift2[A, B, C]:
  (A => B => C) => (F[A] => F[B] => F[C])

  def lift3[A, B, C, D]:
  (A => B => C => D) => (F[A] => F[B] => F[C] => F[D])

  // ... and so on

  // The relationship between lift<N> and lift<N-1>
  // can be given by a function,

  def ap[A, B]:
  F[A => B] => F[A] => F[B]
}

trait LiftImpl[F[_]] extends Lift[F] {
  // Each lift function uses
  // the previous lift function and ap.

  def lift1[A, B]:
  (A => B) => (F[A] => F[B])
  = ap compose lift0

  def lift2[A, B, C]:
  (A => B => C) => (F[A] => F[B] => F[C])
  = f => ap compose lift1(f)

  def lift3[A, B, C, D]:
  (A => B => C => D) => (F[A] => F[B] => F[C] => F[D])
  = f => a => b => ap(lift2(f)(a)(b))
}

// Notes
// * lift0 is often called: unit, return, pure, point, η
// * lift1 is often called: fmap, map, ∘
// * lift<N> is often called: liftA<N>, liftM<N>
