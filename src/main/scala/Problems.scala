
object Problems extends App {


  /*
    P01
    Find the last element of a list.
    Example:
    scala> last(List(1, 1, 2, 3, 5, 8))
    res0: Int = 8
   */
  @scala.annotation.tailrec
  def last[A](xs: List[A]): Option[A] = xs match {
    case Nil => None
    case x :: Nil => Some(x)
    case _ :: xs => last(xs)
  }

  /*
    P02 (*) Find the last but one element of a list.
    Example:
    scala> penultimate(List(1, 1, 2, 3, 5, 8))
    res0: Int = 5
   */
  @scala.annotation.tailrec
  def penultimate[A](xs: List[A]): Option[A] = xs match {
    case Nil => None
    case _ :: Nil => None
    case x :: _ :: Nil => Some(x)
    case _ :: xs => penultimate(xs)
  }

  /*
    P03 (*) Find the Kth element of a list.
    By convention, the first element in the list is element 0.
    Example:

    scala> nth(2, List(1, 1, 2, 3, 5, 8))
    res0: Int = 2
   */
  def nth[A](n: Int, as: List[A]): Option[A] = {
    @scala.annotation.tailrec
    def go(as: List[A], count: Int = 0): Option[A] = as match {
      case x :: _ if count == n => Some(x)
      case _ :: xs => go(xs, count + 1)
    }

    if (as.length < n) None
    else go(as)

  }

  /*
    P04 (*) Find the number of elements of a list.
    Example:
    scala> length(List(1, 1, 2, 3, 5, 8))
    res0: Int = 6
   */
  def length[A](as: List[A]): Int = {
    @scala.annotation.tailrec
    def go(as: List[A], count: Int = 0): Int = as match {
      case Nil => count
      case _::xs => go(xs, count + 1)
    }

    go(as)
  }

  /*
  P05 (*) Reverse a list.
  Example:
    scala> reverse(List(1, 1, 2, 3, 5, 8))
  res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */

  def reverse[A](as: List[A]): List[A] = {
    @scala.annotation.tailrec
    def go(as: List[A], newList: List[A] = Nil): List[A] = as match {
      case Nil => newList
      case x::xs => go(xs, x +: newList)
    }

    as match {
      case Nil => Nil
      case xs => go(xs)

    }

  }




    val xs = List(1, 1, 2, 3, 5, 8)

  print(last(xs))
  print(penultimate(xs))
  print(nth(2, xs))
  print(reverse(xs))

}
