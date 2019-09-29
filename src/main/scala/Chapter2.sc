// annotation.tailrec
// anonymous functions aka inline functions
// polymorphic and monomorphic functions
//

object Solutions{

  def factorial (n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, sum: Int): Int = {
      if (n <= 0) sum
      else go(n - 1, n * sum)
    }
    go(n, 1)
  }

  //Excercise 2.1 -- nth Fibonacci
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, last: Int, curr: Int): Int = {
      if(n <= 1) last
      else go(n-1, curr , last+curr)
    }
    go(n, 0, 1)
  }

  def compare(x: Int, y: Int): Boolean = {
    x<y
  }
  //exercise 2.2
  def isSorted[A](as: Array[A], compare: (A,A)=> Boolean ): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      //println(as.length)
      if( n >= as.length - 1 ) true
      else if(!compare(as(n),as(n+1)))false
      else loop(n + 1)
    }
    loop(0)
  }

  /*
    Anonymous Function
      - a function without name
      - can be used to anonymously pass the function to another defining it inside only
      - eg (x: Int) => x==9  ( Notice Arrow )
   */
  /*
    **TRAIT IS AN INTERFACE
   */
  (x: Int, y:Int) => x==y //warning, no error

  val equalTo = new Function[Int, Int, Boolean] {
    def apply(a: Int, b: Int) = a<b
  }

  //partially applying a function

  def understandFunction[A, B, C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a,b)
      //[B](b: B) not necessary because B is already there
  }


  //exercise 2.3
  def curry[A, B, C](f: (A, B) => C ): A => (B => C) = {
    //curry has three type parameters A,B,C
    // it takes a function that takes A,B and returns C
    // AND returns a function that takes A and returns a function
    // that takes B and returns C

    //Anonymous function
    a: A => b: B => f(a,b)
    // easy to write as
    // a => b => f(a,b)
  }

  // exercise 2.4
  def uncurry[A, B, C](f: A => ( B => C ) ): (A, B) => C = {
    (a,b) => f(a)(b)
  }
  // ex 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a) => f(g(a))
  }
}
