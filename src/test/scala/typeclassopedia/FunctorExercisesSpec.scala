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

  "iTreeFunctor" should {
    "implement map correctly" in {
      def collectResults[A](tree: ITree[A], arg: Int): List[A] = tree match {
        case ILeaf(f) => List(f(arg))
        case INode(trees) => trees.flatMap(x => collectResults(x, arg))
      }

      val iTree = INode(List(
        ILeaf({ _ * 2 }),
        ILeaf({ _ + 1 }),
        INode(List(
          ILeaf({ _ - 10 })
        ))
      ))

      collectResults(iTree, 1) ∘ { _.toString } mustEqual List(
        "2",
        "2",
        "-9"
      )
    }
  }
}
