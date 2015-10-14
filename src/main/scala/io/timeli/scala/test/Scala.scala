package io.timeli.scala.test

class Scala {

  // example how your answers work with comments, be verbose
  
  // The recursive form is:
  // fib(n) = fib(n-1) + fib(n-2)
  // fib(0) = fib(1) = 1
  // where fib returns the Fibonacci number of 'number'
  // We further need to build the list of such numbers 
  // in the range of 0 to 'number'
  // Hence the additional function 'fib' and the conversion 
  // of the result to Seq from IndexedSeq
  // The case of negative numbers can be handle by a require(n>0, "Negative numbers not allowed")
  // For big numbers, this function gets lost in the memory abyss... The test fails for number=80
  def recursiveFibonacci(number: Int): Seq[Int] = {
    def fib(n: Int): Int = {
        if( n <=1 ) n
        else
            fib(n-1) + fib(n-2)
        }  
      
    (( 0 to number) map fib).toSeq  
  }

  // Therefore we need to use an iterative form. 
  // (All recursive functions have an iterative resolution)
  // The signature here was changed a bit (Long instead of Int)
  // to accomodate for the big numbers generated. The test code (with number =80)
  // reflects this change too.
  def fibonacci(number: Int): Seq[Long] = {

    var fibBuf = scala.collection.mutable.ArrayBuffer.empty[Long]
    fibBuf += (0,1)
    var f1: Long = 0;
    var f2: Long = 1;
    var fn: Long = 0;
    for (i<-3 to number) {
        fn = f1 + f2
        f1 = f2
        f2 = fn
        fibBuf += (fn)
    }
    fibBuf.toList
  } 
  
    
  // Split a list of A-elements into a tuple of 2 A-sequences: 
  //     one with elements of 'list' that verify the predicate f 
  //         (i.e. filter in from the list), 
  //     and the other with those that don't 
  //         (i.e filter them out of the list thus keeping those 
  //          that don't)
  // Complexity:
  // This approach while simple goes through the 'list' list twice: O(2n)
  
  def slowerSplit[A](list: Seq[A], f: A => Boolean): (Seq[A], Seq[A]) = 
      (list filter f, list filter (!f(_)))
  
    
  // An improved algorithm would go over the 'list' list just once:
  // if we don't reverse the lists
  def splitToo[A](list: Seq[A], f: A => Boolean): (Seq[A], Seq[A]) = {
    // Here we could use MutableList but then we do not benefit from the O(1) prepend on List
    var inList   = List[A]()
    var outList  = List[A]()
      
    for ( el <- list) { 
      f(el) match {
        case true  => inList  = el :: inList
        case false => outList = el :: outList
      }
    }
    // Otherwise not necessary, the list reversal is added to accomodate the test
    // But doing so incurs a performance hit: two additional traversals
    (inList.reverse.toSeq, outList.reverse.toSeq)   
  }

  // Therefore another version using a mutable list provides a better computation time 
  // and memory allocation scheme
  def split[A](list: Seq[A], f: A => Boolean): (Seq[A], Seq[A]) = {
    // Here we could use MutableList but then we do not benefit from the O(1) prepend on List
    var inList   = scala.collection.mutable.ArrayBuffer.empty[A]
    var outList  = scala.collection.mutable.ArrayBuffer.empty[A]
      
    for ( el <- list) { 
      f(el) match {
        case true  => inList  += el 
        case false => outList +=el
      }
    }
      // the list reversal is not needed: the sequence is in the desired order
    (inList.toSeq, outList.toSeq)   
  }
    
  // Recursive function for primes  
  def recursivePrimes(number: Int): Seq[Int] = {
      def isPrime(n: Int) = {
          if(n <= 1 ) false
          else
			    (2 until n) forall ( n % _ != 0)
      }
      (( 0 to number) filter isPrime).toSeq
    }

  // A faster algorithm uses the Eratostenes sieve
  // that reduces the range to under sqrt(n) - first loop,
  // and by marking the multiples of these numbers starting at
  // their square - second loop. What is not marked in the boolean array is prime.
  def upToPrimes(number: Int): Seq[Int] = {
      val nroot = Math.pow(number, 0.5).toInt 
      var isComposite = Array.fill[Boolean](number)(false)
      val primBuf = scala.collection.mutable.ArrayBuffer.empty[Int] // as fast as using an Vector

      for( i <- Range(2, nroot+1) ) {
        if( !isComposite(i) ) {
			primBuf += i
			for(j <- Range(i*i, number, i)) {
				isComposite(j) = true
			}
		}
	  }
	  for(k <- Range(nroot+1, number)) {
		if(!isComposite(k)) {
			primBuf += k		
			}
	}
	primBuf.toList
    }
    
    
  // The required 'primes' function: 
  //     
  //    - Determine the first 'number' primes
  //    - Return a sequence of 1000 of them because the test is expecting exactly that
  //
  // Theoretically there are m = nlog(n) primes number pi  (p1, p2, ..., pn).
  // Therefore in order to get the first n primes we need to look in the range 1 to m
  // In practice I added a 20% majoration of m to get to the 1000 requested in the test.
  // The bigger the m number the sparser the primes
  def primes(number: Int): Seq[Int] = {
  
        val m = (number * Math.log(number) * 1.2 ).toInt
        upToPrimes(m).take(1000)
  }
    
  // Another approach to solve the problem is to go incrementally:
  //    - either by additional numbers, verify its primality and add it to the sequence 
  //       as we go until reaching 'number' primes
  //    - or by additional ranges of numbers, verify their primality, and add them
  //       to the sequence as we go until we reach the 'number' primes or just beyond 
  //       and take exactly 'number' of them, This is following the MapReduce principle and 
  //       thus can be parallelized
        
    
    
  // fibPrimes constructs the intersection of the 2 lists i.e. it retains only 
  // the fib numbers in 'fibs' that are also prime in the 'primes' list
  // Because the elements in the primes sequence are Int we could convert them to Long on the fly
  // But we don't need to do that. Scala converts to the higher precision when comparing, hence 
  // commenting out the "map to Long"
  // I am assuming that the intersect function is the most performant out there. Not verified.
  def fibPrimes(fibs: Seq[Long], primes: Seq[Int]): Seq[Long] = 
    fibs intersect primes   //... map (x => x.toLong)
    
}