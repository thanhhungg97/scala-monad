package content


trait Monad[M[_]] {
  def pure[A](a: A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))

}


object Example {
  implicit val listMonad = new Monad[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  def exponent(ma: List[Int])(implicit monad: Monad[List]) = {
    monad.flatMap[Int, Int](ma)(x => List(2 * x))
  }

  def main(args: Array[String]): Unit = {
    println(exponent(List(3, 4)))
  }
}