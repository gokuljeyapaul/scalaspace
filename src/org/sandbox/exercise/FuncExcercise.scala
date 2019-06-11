package org.sandbox.exercise

import scala.annotation.tailrec

object FuncExcercise extends App {

  def greet(name: String, age: Integer) : String = s"Hello, my name is ${name} and I am ${age} year(s) old"

  def factorial(n: Integer): Integer = {
    if (n == 0) return 1
    return n * factorial(Math.abs(n)-1)
  }

  /**
   * Tail recursion, use recursion as the last expression in the code path
   * @param n
   * @return
   */
  def accumulatedFactorial(n: BigInt): BigInt = {
    @tailrec
    def factorial(x: BigInt, accumulator: BigInt): BigInt = {
      if (x <= 1) return accumulator
      else factorial(x-1, x * accumulator)
    }
    factorial(n, 1)
  }

  def fibonacci(n: Integer): Integer = {
    if (n < 0) return 0
    if (n == 1) return 1
    if (n == 2) return 1
    return fibonacci(n - 1) + fibonacci(n - 2)
  }

  def tailRecFibonacci(n: Integer): Integer = {
    @tailrec
    def fibonacci(t: Integer, last: Integer, nextLast: Integer): Integer = {
      if (t >= n) return last
      else fibonacci(t+1, last + nextLast, last)
    }
    if (n <= 2) return 1
    else fibonacci(2, 1, 1)
  }

  def isPrime(n : Integer): Boolean = {
    @tailrec
    def isPrimeUntil(t: Integer): Boolean = {
      if (t <= 1) return true
      else n%t !=0 && isPrimeUntil(t-1)
    }
    isPrimeUntil(n/2)
  }

  def tailRecIsPrime(n: Integer) : Boolean = {
    @tailrec
    def isPrimeUntil(t: Integer, accumulator: Boolean): Boolean = {
      if (!accumulator) return false
      else if (t <= 1) return  true
      else isPrimeUntil(t-1, n%t != 0 && accumulator)
    }
    isPrimeUntil(n/2, true)
  }

  def echo(n: Integer, s: String) : String = {
    @tailrec
    def appender(times: Integer, accumulator: String) : String = {
      if (times == 1)  accumulator
      else appender(times - 1, accumulator + "\n" +accumulator)
    }
    appender(n, s)
  }


  //println(greet("Gokul", 35))

  println(fibonacci(8))

  println(tailRecFibonacci(8))

  //println(isPrime(7))

  //println(tailRecIsPrime(7))

  //println(tailRecIsPrime(10))

  //println(accumulatedFactorial(-10))

  //println(echo(2, "Sai Baba"))
}
