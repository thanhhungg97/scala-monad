package content

class Lazy[+A](value: => A) {
  private lazy val internal: A = value

  // need a mechanism to sequence computation over a value wrapper inside a monad
  // must provide flatMap
  def map[B](f: A => B): Lazy[B] = flatMap(x => Lazy(f(x)))

  // any type providing both the map and the flatMap function in Scala, => we can use
  // for-comprehension construct
  def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internal)
  def get: A = internal
}

object Laws {

  // it's not sufficient to add the unit and flatMap functions to a type to make it a monad.
  // we must to fulfill the mathematical laws.
  // Three laws:
  // left identity
  // right identity
  // associativity
  // if the monad satisfies the three laws, the we gurantee that any sequence of applications of the unit and the flatMap function
  // leads to a valid monads

  def leftIdentity[A, B](x: A, f: A => Lazy[B]): Boolean = {
    Lazy(x).flatMap(f) == f(x)
    // if that properties is hold, then we can substitute flatMap(f) with f(x)
  }

  def rightIdentity[A](x: A): Boolean = {
    Lazy(x).flatMap(y => Lazy(y)) == Lazy(x)
  }
  def associativity[A, B, C](x: A, f: A=>Lazy[B], g: B =>Lazy[C] ): Boolean ={
    Lazy(x).flatMap(f).flatMap(g) == f(x).flatMap(g)
  }

}

object Lazy {
  def flatten[A](m: Lazy[Lazy[A]]) = m.flatMap(x => x)
  def apply[A](value: => A): Lazy[A] = new Lazy[A](value)

  def main(args: Array[String]): Unit = {

    val lazyInt: Lazy[Int] = Lazy {
      println("The response to everything is 42")
      42
    }
    // no int will be print to the standard output because of all the computation's laziness.
    val lazyString42 = lazyInt.flatMap {
      intValue => Lazy(intValue.toString)
    }
    val result = for {
      first <- Lazy(1)
      second <- Lazy(2)
      third <- Lazy(3)
    } yield first + second + third
  }
}