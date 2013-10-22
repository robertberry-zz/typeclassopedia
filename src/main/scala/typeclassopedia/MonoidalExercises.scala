package typeclassopedia

import scalaz._

object MonoidalExercises {
  trait Monoidal[F[_]] extends Functor[F] {
    def unit: F[Unit]

    def **[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  /** Some examples */
  implicit object OptionMonoidal extends Monoidal[Option] {
    def unit: Option[Unit] = Some(Unit)

    def **[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
      case (Some(a), Some(b)) => Some((a, b))
      case _ => None
    }

    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit object ListMonoidal extends Monoidal[List] {
    def unit: List[Unit] = List(Unit)

    /** Could be implemented as a zipper or non-determinism */
    def **[A, B](fa: List[A], fb: List[B]): List[(A, B)] = for {
      a <- fa
      b <- fb
    } yield (a, b)

    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit def monoidal2Applicative[F[_]](implicit monoidal: Monoidal[F]) = new Applicative[F] {
    def point[A](a: => A): F[A] = monoidal.map(monoidal.unit)(_ => a)

    def ap[A, B](fa: => F[A])(f: => F[(A) => B]): F[B] = monoidal.map(monoidal.**(f, fa)) {
      case ((g, x)) => g(x)
    }
  }

  implicit def applicative2Monoidal[F[_]](implicit applicative: Applicative[F]) = new Monoidal[F] {
    def unit: F[Unit] = applicative.pure(Unit)

    def **[A, B](fa: F[A], fb: F[B]): F[(A, B)] = applicative.ap(fb)(applicative.map(fa)(a => (b: B) => (a, b)))

    def map[A, B](fa: F[A])(f: (A) => B): F[B] = applicative.map(fa)(f)
  }
}
