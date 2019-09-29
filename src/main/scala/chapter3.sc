
/* Skipped as was difficult to digest
  variadic function
*/


// -- List Data Structure
/*
++ concatenate lists
List is a data type

Keywords
trait is equivalent to a class

class List  === data type
trait List
---class is a data type

+A means A is covariant
Cons is Constructor
Nil is Null

sealed trait
-- it is a constraint that all the different method implementations
will be in the same file only

 */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


//COMPANION OBJECT of LIST
//Contains all the utility functions related to the object
// eg for list -- create, apply, sum, prod
// object of the same name as data type
object List {
  //pattern matching constructors
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
    // x is representing head element and xs the remaining list
    //the sum of non empty list is sum of first ele x and
    // the sum of remaining elements
  }
  def product(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(x,xs) => x*product(xs)
  }
  // List Instantiator function
  // VARIADIC FUNCTION
  // Need explanation
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) // WTF ??
  }

}

// Functions for
val emptyList: List[Int] = Nil
val singleList: List[String] = Cons("1", Nil)
val bigList: List[String] = Cons("a", Cons("b", Nil))


// -- Pattern Matching
/*
  ds or xs is generic type for a list of variables of certain
  data type
*/

// Ex 3.1
// 3rd case although 4th will also match.
// Scala chooses the first matched
// 3
val x = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

// 1 2 3 4 5

// Ex 3.2
def tail[A](as: List[A]): List[A]  = as match {
  case Nil => Nil
  case Cons(_, as) => as
}

// Ex 3.3
def setHead[A](ele: A, as: List[A]): List[A] = as match {
  case Nil => Nil
  case Cons(_, as) => Cons(ele, as)
}
//1 2 3 4 5
// n  = 3
// Ex 3.4
@annotation.tailrec
def drop[A](l: List[A], n: Int): List[A] = l match {
  case Nil => Nil
  case Cons(h, ls)=> if(n==1) ls
  else drop(ls,n-1)
}

// Ex 3.5
// I understood this case
// 4,4,4,4,5,6,5,4  = 5,6,5,4
@annotation.tailrec
def dropWhile[A](l: List[A], f: A => Boolean ): List[A] = l match {
  case Nil => Nil
  case Cons(h, ls) => if(!f(h)) ls
  else dropWhile(ls,f)
}

// Not exercise
def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
  case Nil => a2
  case Cons(h,t) => Cons(h, append(t,a2))
}

// 1 2 3 4  ->  1 2 3
// Ex 3.6
def init[A](as: List[A]): List[A] = as match {
  case Nil => Nil
  case Cons(_,Nil) => Nil
  case Cons(h,t) => Cons(h,init(t))
}

/*3.3.2
 Improving Type inference for Higher Order Functions
 - Not always informing about the type of the inputs that we are giving
 -
*/
def newDropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
  case Cons(h,t) if f(h) => Cons(h,newDropWhile(t,f))
  case _ => xs
}

// ** TYPE Inference occurs from left to right
// eg. to call newDropWhile we do
// xs: List[Int]
// newDropWhile(xs)(x => x < 4)
// newDropWhile generic type A will assume Int as we declare List of Ints and then the next right function doesn't need a type on x


/*
 -- 3.4 RECURSION OVER LISTS AND GENERALIZING TO HIGHER ORDER FUNCTIONS

  FOLDRIGHT
*/


def foldRight[A,B](as: List[A], defValue:B)(f: (A,B) => B): B = as match {
  case Nil => defValue
  case Cons(x,xs) => f(x, foldRight(xs,defValue)(f))
}


def mySum(ns: List[Int]): Int  = {
  foldRight(ns,0)(_+ _) // only when the types can be inferred by scala
}

// (xs,n)=> xs.drop(n)
// _drop_

/*
 ex 3.7
-- our current implementation won't halt
-- but we can modify the FoldRight and implement in the Product
-- Yes we can shortcircuit midway ( short circuit the recursion )
*/
def foldRightProduct(list: List[Double], baseCase: Double)(func: (Double,Double) => Double): Double =
  list match {
  case Nil => baseCase
  case Cons(head, tail) => if(head == 0.0 ) 0.0 else func(head, foldRightProduct(tail, baseCase)(func))
}

def prodUsingModifiedFoldRight(list: List[Double]): Double = {
  foldRightProduct(list, 1.0)(_*_)
}
// Exercise 3.8

//Excercise 3.9
def lengthOfList[A](list: List[A]): Int =  {
  foldRight(list , 0)((_,y) => 1 +  y)
  //1 2 3 4 5
  // head , tail head + fr(cons)(f) Cons(),  Nil = 0
}

//Ex 3.10
@annotation.tailrec
def foldLeft[A,B](list: List[A], defvalue: B)(func: (B,A) => B): B = list match {
  case Nil => defvalue // reached end return def value
  case Cons(h,t) => foldLeft(t, func(defvalue,h))(func)
}

//Ex 3.11
def sum(list: List[Int]): Int = {
  foldLeft(list, 0)( (x, y ) => x+y)
}

def prod(list: List[Int]): Int = {
  foldLeft(list, 1)( _*_)
}

def len(list: List[Int]): Int = {
  foldLeft(list, 0)((x,_) => 1+x )
}

// ex 3.12
// List (1,2,3) ==> List(3,2,1)


// 3.13 -


// 3.14
// Append using foldLeft
//append(Cons(1,Cons(2,Cons(3,Nil))),Cons(4,Cons(5,Cons(6,Nil))))
// ==> Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))
def append[A](l1:List[A], l2:List[A]): List[A] = {
  foldRight(l1, l2)((x,y) => Cons(x,y))
}

// 3.15
// List(List(1,2,3), List(4,5,6), Nil) --> List(1,2,3,4,5,6, Nil)
def concatenate[A](listOfLists: List[List[A]]): List[A] = {
  foldRight(listOfLists, Nil: List[A])((x,y) => foldRight(x,y)((a,b) => Cons(a,b)))
 }

// ex 3.16
// 1 2 3 4 = 2 3 4 5
def addOne(myList: List[Int]): List[Int]  = {
  foldRight(myList, Nil: List[Int])( (p,q) => Cons(p+1,q))
}

//--using fold
//3.17
def toString(myList: List[Double]): List[String] = {
  foldRight(myList, Nil: List[String])((h,t)=> Cons(h.toString,t))
}

//3.18
def map[A,B](list: List[A])(f: A => B): List[B] = {
  foldRight(list, Nil: List[B])((a,b) => Cons(f(a),b))
}
def isOdd(a: Int): Boolean = {
  if(a%2 == 0) false
  else true
}

//3.19
def filter[A](list: List[A])(f: A => Boolean): List[A] = {
  foldRight(list, Nil: List[A])( (a, b) => if(f(a)) Cons(a,b) else b)
}

//3.20
def flatMap[A,B](list: List[A])(f: A => List[B]): List[B] = {
  foldRight(list, Nil: List[B])((h,t) => {
     val tempList = f(h)
     foldRight(tempList, t)((x,y) => Cons(x,y))
  })
}

//3.21
def filterUsingFlatMap[A](list: List[A])(f: A => Boolean): List[A] = {

}

//3.22
def addLists[A](list1: List[A], list2: List[A]): List[A] = {
  //assuming same size of the lists

}

//3.23
def zipWith[A,B](list1: List[A], list2: List[A])(f: A => B): List[B] = {

}

//3.24
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

}

//  TREES
sealed trait class Tree
//3.25


//3.26


//3.27

//3.28

//3.29

