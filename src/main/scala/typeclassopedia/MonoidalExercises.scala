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
}
