package typeclassopedia

import scalaz._

object FunctorExercises {
  /** Instances */
  /** --------- */

  /** You have to use ({type l[D] = Either[C, D]})# here, as Either[C, _] is not considered by the compiler to be F[_].
    * This is because of an isomorphism between types and functions in Haskell. All functions in Haskell take one
    * parameter - multi-parameter functions are generalized under this as functions that take one parameter, then
    * return other functions. This holds true for types in Haskell, too. Neither is true in Scala by default (although
    * Scala does offer a syntax for curried functions; this is the closest thing we have for types).
    */
  implicit def eitherFunctor[C] = new Functor[({type l[D] = Either[C, D]})#l] {
    def map[A, B](fa: Either[C, A])(f: (A) => B): Either[C, B] = fa match {
      case Right(x) => Right(f(x))
      case Left(error) => Left(error)
    }
  }

  implicit def function2Functor[C] = new Functor[({type l[D] = C => D})#l] {
    def map[A, B](fa: C => A)(f: (A) => B): C => B = f compose fa
  }

  implicit def tupleFunctor[C] = new Functor[({type l[D] = (C, D)})#l] {
    def map[A, B](fa: (C, A))(f: (A) => B): (C, B) = (fa._1, f(fa._2))
  }

  case class Pair[A](a1: A, a2: A)

  implicit def pairFunctor = new Functor[Pair] {
    def map[A, B](fa: Pair[A])(f: (A) => B): Pair[B] = fa match {
      case Pair(a1, a2) => Pair(f(a1), f(a2))
    }
  }

  /** Difference is you need to apply the tupleFunctor for the type of the first element, whereas in Pair both elements
    * are of that type. This also means that in Pair the functor maps over both contained values, whereas in tuple the
    * functor maps over only the right value
    */

  /** Not sure what this funky datatype is supposed to be ... */
  sealed trait ITree[A]
  case class ILeaf[A](f: Int => A) extends ITree[A]
  case class INode[A](trees: List[ITree[A]]) extends ITree[A]

  implicit def iTreeFunctor = new Functor[ITree] {
    def map[A, B](fa: ITree[A])(f: (A) => B): ITree[B] = {
      def iter(node: ITree[A]): ITree[B] = node match {
        case ILeaf(leaf) => ILeaf(f compose leaf)
        case INode(trees) => INode(trees.map(iter))
      }
      iter(fa)
    }
  }

  type FunctionAInt[A] = Function1[A, Int]
  /** This can't be a functor because fmap must accept a transformation from A => B. As FunctionAInt only accepts an A
    * rather than enclosing or producing one, there's no sensible way to perform this mapping.
    *
    * i.e., the only way for fmap id == id is if fmap is essentially a no-op
    */

  /** The composition of two functors is a functor? */


  /** Laws */
  /** ---- */

  case class Switch[A](a: A, switch: Boolean)

  implicit def evilFunctor = new Functor[Switch] {
    def map[A, B](fa: Switch[A])(f: (A) => B): Switch[B] = fa match {
      case Switch(a, switch) => Switch(f(a), true)
    }
  }
}
