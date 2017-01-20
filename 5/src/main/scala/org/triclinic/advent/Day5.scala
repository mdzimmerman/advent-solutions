package org.triclinic.advent

import scala.io.Source

/**
  * Created by Matt on 1/24/2016.
  */
object Day5 extends App {
  def subStrings(s: String) = {
    val l = s.length
    s.sliding(2).toList
    //(0 until l-1).map(i => s.substring(i,i+2))
  }

  def doubledChars(s: String) = {
    val sl = s.toList
    sl(0) == sl(1)
  }

  def isVowel(c: Char) = {
    val vowels = List('a', 'e', 'i', 'o', 'u')
    if (vowels.contains(c)) 1 else 0
  }

  def isBlackList(s: String) = {
    val blackList = List("ab", "cd", "pq", "xy")
    blackList.contains(s)
  }

  def part1(filename: String) = {
    var total = 0
    for (line <- Source.fromFile(filename).getLines) {
      print(line)

      val hasDouble = line.sliding(2).toList.exists(x => doubledChars(x))
      print(" %5s".format(hasDouble))

      val count = line.toList.map(isVowel(_)).sum
      val hasEnoughVowels = (count >= 3)
      print(" %2d %5s".format(count, hasEnoughVowels))

      val hasBlackList = line.sliding(2).toList.exists(x => isBlackList(x))
      print(" %5s".format(hasBlackList))

      println()

      if (hasDouble && hasEnoughVowels && !hasBlackList)
        total += 1
    }

    println("Total: %d".format(total))
  }

  def part2rule1(s: String): Boolean = {
    val len = s.length
    for (i <- 0 until len-3) {
      val a = s.substring(i, i+2)
      //print(s"$a: ")
      for (j <- i+2 until len-1) {
        val b = s.substring(j, j+2)
        //print(s"$b ")
        if ( a == b ) {
          //println("MATCH")
          return true
        }
      }
      //println()
    }
    false
  }

  def part2rule2(s: String): Boolean = {
    s.toSeq.sliding(3).exists(x => x(0) == x(2))
  }

  def part2(filename: String) = {
    var total = 0
    for (line <- Source.fromFile(filename).getLines) {
      val rule1 = part2rule1(line)
      val rule2 = part2rule2(line)
      val valid = rule1 && rule2
      println(s"$line $rule1 $rule2 $valid")
      if (valid) total += 1
    }
    println("Total: %d".format(total))
  }

  override def main(args: Array[String]): Unit = {
    //args.foreach(println(_))
    //part1(args(0))
    part2(args(0))
  }
}
