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

  /** Has to be ZipSeq for the point method below. In Haskell, this can just be a List due to laziness being a built-in
    * feature. Infinite Lists are not possible in Scala, but infinite Streams are. Both extend Seq.
    */
  sealed trait ZipSeq

  def ZipSeq[A](as: Seq[A]): Seq[A] @@ ZipSeq = Tag[Seq[A], ZipSeq](as)

  implicit val zipSeqApplicative = new Applicative[({type l[A] = Seq[A] @@ ZipSeq})#l] {
    def point[A](a: => A) = ZipSeq(Stream.continually(a))

    def ap[A, B](fa: => Seq[A] @@ ZipSeq)(f: => Seq[A => B] @@ ZipSeq): Seq[B] @@ ZipSeq = ZipSeq(for {
      a <- fa
      func <- f
    } yield func(a))
  }
}
