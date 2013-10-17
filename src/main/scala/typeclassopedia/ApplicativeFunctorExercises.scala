package typeclassopedia

import scalaz._

object ApplicativeFunctorExercises {
  /** Option rather than Maybe here - but yes, trivial */
  val optionApplicative = new Applicative[Option] {
    def point[A](a: => A): Option[A] = Some(a)

    def ap[A, B](fa: => Option[A])(f: => Option[(A) => B]): Option[B] = for {
      func <- f
      a <- fa
    } yield func(a)
  }

  /** Has to be Seq for the point method below. In Haskell, this can just be a List due to laziness. Infinite Lists
    * are not possible in Scala, but infinite Streams are. Both extend Seq.
    */
  sealed trait ZipStream

  def ZipStream[A](as: A): A @@ ZipStream = Tag[A, ZipStream](as)

  implicit val zipStreamApplicative: Applicative[({type l[A] = Stream[A] @@ ZipStream})#l] = new Applicative[({type l[A] = Stream[A] @@ ZipStream})#l] {
    def point[A](a: => A) = ZipStream(Stream.continually(a))

    def ap[A, B](fa: => Stream[A] @@ ZipStream)(f: => Stream[A => B] @@ ZipStream): Stream[B] @@ ZipStream =
      ZipStream(f.zip(fa).map({case (g, x) => g(x)}))
  }
}
