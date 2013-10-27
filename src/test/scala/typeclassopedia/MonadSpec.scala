package typeclassopedia

import org.scalacheck._
import Prop._
import MonadExercises._
import org.scalacheck.Arbitrary._
import org.scalacheck.util.Buildable

object ListMonadSpec extends Properties("Monad") {
  property("point") = forAll { (i: Int) =>
    ListMonad.point(i) == List(i)
  }

  property("bind") = forAll { (xs: List[Int]) =>
    def precIdAndSucc(i: Int) = List(i - 1, i, i + 1)

    xs.flatMap(precIdAndSucc) == ListMonad.bind(xs)(precIdAndSucc)
  }
}

object ReaderMonadSpec extends Properties("Monad") with FunctionIntIntHelper {
  property("point") = forAll { (i: Int, j: Int) =>
    ReaderMonad.point(i)(j) == i
  }

  property("bind") = forAll { (f: Int => Int, g: Int => Int, i: Int) =>
    def h(x: Int) = (y: Int) => g(y) + y

    ReaderMonad.bind(f)(h)(i) == h(f(i))(i)
  }
}

trait FreeHelper {
  implicit def arbitraryFree[F[_], A](implicit buildable: Buildable[Free[F, A], F], arbitraryA: Arbitrary[A]) = Arbitrary {
    lazy val genVar = for {
      f <- arbitrary[A]
    } yield Var[F, A](f)

    lazy val genNode = for {
      i <- Gen.chooseNum(1, 3) /* To prevent stack overflows */
      elems <- Gen.containerOfN[F, Free[F, A]](i, genFree)
    } yield Node(elems)

    def genFree: Gen[Free[F, A]] = Gen.oneOf(genVar, genNode) map { case x: Free[F, A] => x }

    genFree
  }

  def flatten(free: Free[List, Int]): List[Int] = free match {
    case Var(n) => List(n)
    case Node(frees) => frees.flatMap(flatten)
  }
}

object FreeFunctorSpec extends Properties("Functor") with FreeHelper with FunctionIntIntHelper {
  property("map") = forAll { (free: Free[List, Int], f: Int => Int) =>
    flatten(FreeFunctor[List].map(free)(f)) == flatten(free).map(f)
  }
}

object FreeMonadSpec extends Properties("Monad") with FreeHelper {
  property("point") = forAll { (i: Int) =>
    FreeMonad[List].point(i) == Var[List, Int](i)
  }

  property("bind") = forAll { (free: Free[List, Int]) =>
    def f(i: Int): Free[List, Int] = if (i % 2 == 1) {
      Var[List, Int](i)
    } else {
      Node[List, Int](List.fill(5)(Var(i)))
    }

    flatten(FreeMonad[List].bind(free)(f)) == flatten(free).map(f).flatMap(flatten)
  }
}