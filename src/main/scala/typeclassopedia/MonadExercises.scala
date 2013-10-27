package typeclassopedia

import scalaz.{Functor, Monad}

object MonadExercises {
  implicit object ListMonad extends Monad[List] {
    def point[A](a: => A): List[A] = List(a)

    def bind[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa.flatMap(f)
  }

  implicit def ReaderMonad[C] = new Monad[({type l[A] = Function[C, A]})#l] {
    def point[A](a: => A): (C) => A = _ => a

    def bind[A, B](fa: (C) => A)(f: (A) => (C) => B): (C) => B = (c: C) => f(fa(c))(c)
  }

  sealed trait Free[F[_], A]
  case class Var[F[_], A](a: A) extends Free[F, A]
  case class Node[F[_], A](f: F[Free[F, A]]) extends Free[F, A]

  implicit def FreeFunctor[F[_]: Functor] = new Functor[({type l[A] = Free[F, A]})#l] {
    def map[A, B](fa: Free[F, A])(f: (A) => B): Free[F, B] = fa match {
      case Var(a) => Var(f(a))
      case Node(fb) => Node(implicitly[Functor[F]].map(fb)(fc => map(fc)(f)))
    }
  }

  implicit def FreeMonad[F[_]: Functor] = new Monad[({type l[A] = Free[F, A]})#l] {
    def point[A](a: => A): Free[F, A] = Var(a)

    def bind[A, B](fa: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] = fa match {
      case Var(a) => f(a)
      case Node(fb) => Node(implicitly[Functor[F]].map(fb)(fc => bind(fc)(f)))
    }
  }

  def >>=[F[_], A, B](fa: F[A], f: A => F[B])(implicit monad: Monad[F]): F[B] = monad.join(monad.map(fa)(f))

  def join[F[_]: Monad, A](ffa: F[F[A]]): F[A] = >>=(ffa, identity[F[A]])

  /** Would have used compose here but the compiler seems to be confused by it (i.e., monad.point[B].compose(f)) */
  def fmap[F[_], A, B](fa: F[A])(f: A => B)(implicit monad: Monad[F]): F[B] = >>=(fa, (x: A) => monad.point[B](f(x)))
}
