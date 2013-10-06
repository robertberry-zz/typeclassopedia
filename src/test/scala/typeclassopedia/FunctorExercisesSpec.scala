package typeclassopedia

import scalaz.Scalaz._
import FunctorExercises._
import org.specs2.mutable.Specification

class FunctorExercisesSpec extends Specification {
  "eitherFunctor" should {
    "implement map correctly" in {
      val left: Either[String, Int] = Left("hi")

      left ∘ { _ * 2 } mustEqual Left("hi")

      val right: Either[String, Int] = Right(6)

      right ∘ { _ * 2 } mustEqual Right(12)
    }
  }

  "function2Functor" should {
    "implement map correctly" in {
      val f: Int => Int = { _ * 2 }

      val g = f ∘ { _ + 10 }

      g(5) mustEqual 20
    }
  }

  "tupleFunctor" should {
    "implement map correctly" in {
      val tuple = (5, "hello")

      tuple ∘ { _ + " world" } mustEqual (5, "hello world")
    }
  }

  "pairFunctor" should {
    "implement map correctly" in {
      val pair = Pair(10, 45)

      pair ∘ { _ / 5 } mustEqual Pair(2, 9)
    }
  }
}
