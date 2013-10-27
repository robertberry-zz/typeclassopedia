package typeclassopedia

import org.scalacheck._
import Prop._
import MonadExercises._
import org.scalacheck.Arbitrary._
import org.scalacheck.util.Buildable
import scalaz.Applicative

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

trait OptionHelper {
  implicit def arbitraryOption[A: Arbitrary] = Arbitrary {
    for {
      isFull <- arbitrary[Boolean]
    } yield if (isFull) for {
      a <- arbitrary[A]
    } yield Some(a) else None
  }
}

object MonadSpec extends Properties("Monad") with FunctionIntIntHelper with OptionHelper {
  property(">>=") = forAll { (xs: List[Int]) =>
    def f(x: Int) = List.fill(x % 5)(x)

    ListMonad.bind(xs)(f) == >>=(xs, f)
  }

  property("join") = forAll { (xs: List[List[Int]]) =>
    ListMonad.join(xs) == join(xs)
  }

  property("fmap") = forAll { (xs: List[Int], f: Int => Int) =>
    fmap(xs)(f) == ListMonad.map(xs)(f)
  }

  property("ap") = forAll { (xs: List[Int], fs: List[Int => Int]) =>
    ap(fs)(xs) == implicitly[Applicative[List]].ap(xs)(fs)
  }

  property("sequence") = forAll { (opts: List[Option[Int]]) =>
    if (opts.forall(_.isDefined)) {
      sequence(opts) == Some(opts.flatten)
    } else {
      sequence(opts) == None
    }
  }

  property("replicate") = forAll(Gen.chooseNum(1, 100)) { (n: Int) =>
    forAll { (opt: Option[Int]) =>
      replicateM(n)(opt) == (if (opt.isDefined) Some(List.fill(n)(opt).flatten) else None)
    }
  }
}