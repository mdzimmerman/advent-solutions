import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by mzimmerman on 1/31/17.
  */

val quote = """(")(.*)""".r
val escapedQuote = """(\\")(.*)""".r
val escapedSlash = """(\\\\)(.*)""".r
val escapedHex   = """(\\x..)(.*)""".r

def countDequote(s: String): Int = countDequote(s, 0)

@tailrec
def countDequote(s0: String, n0: Int): Int = {
  //println(s"$s0 => $n0")
  if (s0.isEmpty)
    n0
  else {
    val (s, n) = s0 match {
      case escapedQuote(head, tail) => (tail, n0+1)
      case escapedSlash(head, tail) => (tail, n0+1)
      case escapedHex(head, tail) => (tail, n0+1)
      case quote(head, tail) if n0 == 0 || s0.length == 1 => (tail, n0)
      case _ => (s0.tail, n0+1)
    }
    countDequote(s, n)
  }
}

def countEnquote(s: String): Int = countEnquote(s, 0)

@tailrec
def countEnquote(s0: String, n0: Int): Int = {
  //println(s"$s0 => $n0")
  if (s0.isEmpty)
    n0+2
  else {
    val (s, n) = s0 match {
      case escapedQuote(head, tail) => (tail, n0+4) // \" -> \\\"
      case escapedSlash(head, tail) => (tail, n0+4) // \\ -> \\\\
      case escapedHex(head, tail) => (tail, n0+5)   // \xaa -> \\xaa
      case quote(head, tail) => (tail, n0+2)        // " -> \"
      case _ => (s0.tail, n0+1)
    }
    countEnquote(s, n)
  }
}

def diffDequote(s: String): Int = {
  val full = s.length
  val quote = countDequote(s)
  full-quote
}

def diffEnquote(s: String): Int = {
  val full = s.length
  val quote = countEnquote(s)
  quote-full
}

println("--- test ---")
val test = Source.fromFile("test.txt").getLines.toList
println("dequote = "+test.map(diffDequote).sum)
println("enquote = "+test.map(diffEnquote).sum)
println()

println("--- input ---")
val input = Source.fromFile("input.txt").getLines.toList
println("dequote = "+input.map(diffDequote).sum)
println("enquote = "+input.map(diffEnquote).sum)
println()


