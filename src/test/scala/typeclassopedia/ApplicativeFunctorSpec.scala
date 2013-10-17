package typeclassopedia

import org.scalacheck._
import Prop._
import scalaz._
import scalaz.Scalaz._
import ApplicativeFunctorExercises._

object OptionApplicativeSpec extends Properties("OptionApplicative") with FunctionIntIntHelper {
  property("point") = forAll { f: Function[Int, Int] =>
    optionApplicative.point(f) == Some(f)
  }

  property("<*>") = forAll { (f: Function[Int, Int], x: Int) =>
    optionApplicative.ap(Some(x))(optionApplicative.point(f)) == Some(f(x)) &&
    optionApplicative.ap(None)(optionApplicative.point(f)) == None &&
    optionApplicative.ap(Some(x))(None) == None &&
    optionApplicative.ap(None)(None) == None
  }
}

object ZipSeqApplicativeSpec extends Properties("ZipSeqApplicative") with FunctionIntIntHelper {
  property("point") = forAll { (f: Function[Int, Int]) =>
    val pointed = zipStreamApplicative.point(f)

    pointed take 100 forall { _ == f }
  }

  property("<*>") = forAll { (f: Function[Int, Int], xs: List[Int]) =>
    val seq = ZipStream(xs.toStream)

    /* For some reason this only seems to work if I call the applicative typeclass directly. If I try to use implicit
     * resolution it seems to pick up some other typeclass for <*>, causing an infinite loop. Haven't been able to
     * figure out what typeclass or why, though.
     */
    zipStreamApplicative.ap(seq)(zipStreamApplicative.point(f)) == xs.map(f)
  }
}