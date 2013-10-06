package typeclassopedia

import scalaz._

object FunctorExercises {
  implicit def eitherFunctor[C] = new Functor[({type l[D] = Either[C, D]})#l] {
    def map[A, B](fa: Either[C, A])(f: (A) => B): Either[C, B] = fa match {
      case Right(x) => Right(f(x))
      case Left(error) => Left(error)
    }
  }


}
