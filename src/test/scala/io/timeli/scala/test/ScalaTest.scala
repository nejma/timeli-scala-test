package io.timeli.scala.test

import org.scalatest.FunSuite
import org.scalatest.Matchers._
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FibonacciTest extends FunSuite {

  test("Fibonacci and Prime") {

    val sc = new Scala()

    // generate a list of the first 80 Fibonacci numbers
    val fibs = sc.fibonacci(80)
    fibs should have size 80
    fibs.take(10) should be(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))

    // split that list of fibonacci numbers into a list of 
    // even numbers and a list of odd numbers
    val (evens, odds) = sc.split(fibs, ((x: Long) => (x % 2) == 0) )
    evens should have size 27
    odds should have size 53
    evens.take(5) should be(List(0, 2, 8, 34, 144))
    odds.take(5) should be(List(1, 1, 3, 5, 13))

    // generate a list of the first 1000 prime numbers
    val primes = sc.primes(1000)
    primes should have size 1000
    primes.take(10) should be(List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))

    // generate a list of numbers, from the collections above, that are both
    // primes and fibonacci numbers and print them to the console 
    val fibPrimes = sc.fibPrimes(fibs, primes)
    fibPrimes should be(List(2, 3, 5, 13, 89, 233, 1597))
    
    info("All tests passed!")
  }

}