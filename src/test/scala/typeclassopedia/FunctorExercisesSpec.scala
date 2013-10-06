package typeclassopedia

import scalaz.Scalaz._
import FunctorExercises._
import org.specs2.mutable.Specification

class FunctorExercisesSpec extends Specification {
  "EitherFunctor" should {
    "implement map correctly" in {
      val left: Either[String, Int] = Left("hi")

      left ∘ { _ * 2 } mustEqual Left("hi")

      val right: Either[String, Int] = Right(6)

      right ∘ { _ * 2 } mustEqual Right(12)
    }
  }
}
