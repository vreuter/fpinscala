package fpinscala.datastructures

/* `List` data type, parameterized on a type, `A` */
sealed trait List[+A]
// A `List` data constructor representing the empty list
case object Nil extends List[Nothing]
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  /* A function that uses pattern matching to add up a list of integers */
  def sum(ints: List[Int]): Int = ints match {
    // The sum of the empty list is 0.
    case Nil => 0
    // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /* Exercises & any other personal implementations after this */

  def tail[A](l: List[A]): List[A] = l match {
    // TODO: determine if length-1 list will be handled here.
    case Cons(_, as) => as
    // TODO: also determine if Nil is valid as return. Even if so, would empty list be better for Nil input?
    case Nil => Nil
  }

  /* Update value of head element of list. */
  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  /* Shed the first n elements of a list. */
  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(i: Int, t: List[A]): List[A] = {
      if (i == n) t
      else loop(i + 1, tail(t))
    }
    loop(0, l)
  }

  // Drop elements from list PREFIX that match predicate
  // A slightly better design may be to define this i.t.o. logical complement.
  // Then, the first occurrence of false for Boolean would indicate termination.
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    /* Note that boolean evaluation is single-element. */
    @annotation.tailrec
    def go(sublist: List[A]): List[A] = sublist match {
      // Guard against a predicate that loop infinitely.
      // Such a loop could be defined with a function argument
      // that evaluates to true for
      // TODO: determine just how pattern matching works here, with regard to 0- or 1-element list.
      case Cons(a, as) => {
        if (f(a)) go(as)
        else sublist
      }
      case Nil => Nil
    }
    go(l)
  }

  def init[A](l: List[A]): List[A] = {
    def loop(prefix: List[A], remaining: List[A]): List[A] =
      remaining match {
        // Handle case in which empty list is passed as initial argument
        case Nil => Nil
        case Cons(a, Nil) => prefix
        case Cons(a, as) => loop(prefix :+ a, as)
    }
    loop(Nil, l)
  }

  def length[A](l: List[A]): Int = sys.error("todo")

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")

}
