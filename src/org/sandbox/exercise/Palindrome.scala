package org.sandbox.exercise

import scala.annotation.tailrec
import scala.collection.mutable

object Palindrome extends App {

  def tailRecIsPalindrome(str: String): Boolean = {
    @tailrec
    def isPalindrome(chars: List[Char], matches: Boolean): Boolean = {
      if (chars.length <= 1) return matches
      if (matches) return true
      else isPalindrome(chars.drop(1), matches || (chars == chars.reverse))
    }
    isPalindrome(str.toLowerCase().toList, false)
  }

  println(tailRecIsPalindrome("Gokul"))
  println(tailRecIsPalindrome("Nitin"))
  println(tailRecIsPalindrome("Malayalam"))


  def tailRecPalindromicStrings(str: String): String = {
    @tailrec
    def getPalindrome(listOfChars: List[Char], accumulator: String) : String = {
      if (listOfChars.length == 0) return accumulator
      getPalindrome(listOfChars.drop(1), filterPalindromes(listOfChars, accumulator))
    }

    @tailrec
    def filterPalindromes(origChars: List[Char], collector: String): String = {
      if (origChars.length == 0) return collector
      filterPalindromes(origChars.dropRight(1), if (origChars == origChars.reverse) collector + "\n" + origChars.mkString else collector)
    }
    getPalindrome(str.toList, if(str.toLowerCase.toList == str.toLowerCase.toList.reverse) str else "")
  }

  println(tailRecPalindromicStrings("Nitin"))
  println(tailRecPalindromicStrings("Gokol"))
  println(tailRecPalindromicStrings("Geeks"))

}
