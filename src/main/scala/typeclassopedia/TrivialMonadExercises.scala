package typeclassopedia

/**
 * These questions from http://blog.sigfpe.com/2007/04/trivial-monad.html
 */
object TrivialMonadExercises {
  case class W[A](a: A)

  def return_[A](a: A) = W(a)

  def bind[A, B](f: A => W[B]): W[A] => W[B] = {
    case W(x) => f(x)
  }

  def fmap[A, B](f: A => B): W[A] => W[B] = bind((return_[B] _).compose(f))

  /** Q1 **/
  def g(i: Int)(wj: W[Int]): W[Int] = wj match {
    case W(j) => return_(i + j)
  }

  def g2(i: Int) = bind((j: Int) => return_(i + j))

  /** Q2 **/

  def h(wi: W[Int])(wj: W[Int]): W[Int] = {
    val wrappedF = bind((i: Int) => return_(bind((j: Int) => return_(i + j))))

    val wrappedInner = wrappedF(wi)

    bind((f: W[Int] => W[Int]) => f(wj))(wrappedInner)
  }

  /** Although the above works it seems kind of hideous / difficult to read. Can I reduce the complexity? */
}
