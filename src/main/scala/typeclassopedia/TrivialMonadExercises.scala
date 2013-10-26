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
    val arity1 = bind((i: Int) => return_(bind((j: Int) => return_(i + j))))(wi)
    bind((f: W[Int] => W[Int]) => f(wj))(arity1)
  }

  /** Q3 **/

  /**
    * Left identity (return a >>= f === f a):
    *   Given a is of type A, f must be of type A => W[B].
    *   return a will wrap a in W, producing W[A]. bind takes the wrapped value inside of W[A] (the original a) and
    *   applies f to it. Thus, they are equivalent.
    *
    * Right identity (return >>= m === m)
    *   As return simply wraps a value in W, and >>= unwraps a value in W then applies f, binding return is a no-op,
    *    so this is equivalent
    *
    * Associativity ((m >>= f) >>= g === m >>= (\x -> f x >>= g))
    *   As all >>= does is apply f to the value wrapped by m, the order in which binds occur does not matter - only
    *   the order of function application. On the left hand side, f is applied, then g. On the right hand side, f is
    *   applied, then g.
    */

  /** Q4 **/

  def join[A](m: W[W[A]]): W[A] = bind[W[A], A](identity)(m)
}