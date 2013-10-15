package typeclassopedia

import org.scalacheck._
import Prop._
import scalaz._
import scalaz.Scalaz._
import ApplicativeFunctorExercises._

object ZipSeqApplicativeSpec extends Properties("ZipSeqFunctor") with FunctionIntIntHelper {
  property("point") = forAll { (f: Function[Int, Int], xs: List[Int]) =>
    /* Currently fails compilation with error:

    [error] ... could not find implicit value for parameter F0: scalaz.Apply[Seq]
    [error]     ((ZipSeq(xs): Seq[Int] @@ ZipSeq) <*> f.point) == xs.map(f)

    Curious ...
     */
    ((ZipSeq(xs): Seq[Int] @@ ZipSeq) <*> f.point) == xs.map(f)
  }
}