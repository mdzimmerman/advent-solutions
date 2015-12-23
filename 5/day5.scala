/**
  * Created by mzimmerman on 12/18/15.
  */

import scala.io.Source

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

def part2(filename: String) = {
  for (line <- Source.fromFile(filename).getLines) {
    println(line)
    val l = line.length
    for (i <- (0 until l-3)) {
      print(line.substring(i,i+2)+": ")
      for (j <- (i+2 until l-1)) {
        print(line.substring(j,j+2)+" ")
      }
      println()
    }
  }
}

//part1("input.txt")
part2("input.txt")
