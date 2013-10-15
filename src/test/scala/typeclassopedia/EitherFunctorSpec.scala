package typeclassopedia

import scalaz.Scalaz._
import FunctorExercises._
import org.scalacheck._
import Arbitrary.arbitrary
import org.scalacheck.Prop._

object EitherFunctorSpec extends Properties("EitherFunctor") {
  property("fmap") = forAll {either: Either[String, Int] => (either ∘ { _ * 2 }) == (either match {
    case left: Left[String, Int] => left
    case Right(x) => Right(x * 2)
  })}
}

object Function2FunctorSpec extends Properties("Function2Functor") {
  property("fmap") = forAll { (i: Int, j: Int, k: Int) =>
    val f: Int => Int = { _ * i }
    val g: Int => Int = { _ + j }

    (f ∘ g)(k) == g(f(k))
  }
}

object TupleFunctorSpec extends Properties("TupleFunctor") {
  property("fmap") = forAll { (tuple: (Int, String), str: String) =>
    (tuple ∘ { _ + str })._2 == s"${tuple._2}$str"
  }
}

object PairFunctorSpec extends Properties("PairFunctor") {
  implicit def arbitraryPair = Arbitrary {
    for {
      i <- Gen.chooseNum(-500, 500)
      j <- Gen.chooseNum(-500, 500)
    } yield Pair(i, j)
  }

  property("fmap") = forAll { (pair: Pair[Int], i: Int) =>
    pair ∘ { _ + i } == Pair(pair.a1 + i, pair.a2 + i)
  }
}

object ITreeFunctorSpec extends Properties("ITreeFunctor") with FunctionIntIntHelper {
  implicit def arbitraryITreeInt = Arbitrary {
    val genLeaf = for {
      f <- arbitrary[Function[Int, Int]]
    } yield ILeaf(f)

    def genNode: Gen[INode[Int]] = for {
      numberTrees <- Gen.chooseNum(0, 3) /* This to prevent stack overflows */
      trees <- Gen.containerOfN[List, ITree[Int]](numberTrees, genTree)
    } yield INode(trees)

    def genTree: Gen[ITree[Int]] = Gen.oneOf(genLeaf, genNode)

    genTree
  }

  property("fmap") = forAll { (iTree: ITree[Int], f: Function[Int, Int], i: Int) =>
    def leaves[A](tree: ITree[A]): List[Function[Int, A]] = tree match {
      case ILeaf(func) => List(func)
      case INode(trees) => trees.flatMap(leaves)
    }

    leaves(iTree ∘ f).map(_(i)) == leaves(iTree).map(f compose _).map(_(i))
  }
}

object EvilFunctorSpec extends Properties("EvilFunctor") {
  implicit def arbitrarySwitch = Arbitrary {
    for {
      i <- arbitrary[Int]
    } yield Switch(i, false)
  }

  property("composition") = forAll { (switch: Switch[Int], f: Function[Int, Int], g: Function[Int, Int]) =>
    switch ∘ (f compose g) == ((switch ∘ g) ∘ f)
  }

  property("!identity") = forAll { switch: Switch[Int] =>
    switch ∘ identity != identity(switch)
  }
}

