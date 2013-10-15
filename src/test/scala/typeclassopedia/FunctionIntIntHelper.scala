package typeclassopedia

import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary

trait FunctionIntIntHelper {
  type FII = Function[Int, Int]

  implicit def arbitraryFunction2IntInt = Arbitrary {
    for {
      i <- arbitrary[Int]
      f <- Gen.oneOf({ _ + i }: FII, { _ - i }: FII, { _ * i }: FII)
    } yield f
  }
}
