package typeclassopedia

import org.scalacheck._
import Prop._
import Arbitrary.arbitrary
import typeclassopedia.TrivialMonadExercises._

object TrivialMonadExercisesSpec extends Properties("Monad") {
  implicit val arbitraryWInt = Arbitrary {
    for {
      i <- arbitrary[Int]
    } yield return_(i)
  }

  property("bind") = forAll { (i: Int, j: W[Int]) =>
    g(i)(j) == g2(i)(j)
  }

  property("bind") = forAll { (wi: W[Int], wj: W[Int]) =>
    val W(i) = wi
    val W(j) = wj
    h(wi)(wj) == W(i + j)
  }

  property("join") = forAll { (wrapped: W[Int]) =>
    join(return_[W[Int]](wrapped)) == wrapped
  }
}
