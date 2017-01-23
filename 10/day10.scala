import scala.annotation.tailrec

/**
  * Created by mzimmerman on 1/20/17.
  */

def lookAndSay(s: String): String = {
  val t = lookAndSay(s.tail, s.head, 1, "")
  println(t.length)
  t
}

@tailrec
def lookAndSay(s: Seq[Char], c0: Char, n: Int, out: String): String = {
  if ( s.isEmpty )
    out + n.toString + c0
  else {
    if (s.head == c0) {
      lookAndSay(s.tail, c0, n + 1, out)
    } else {
      lookAndSay(s.tail, s.head, 1, out + n.toString + c0)
    }
  }
}

def look(s: String) = s.foldLeft(List.empty[(Char,Int)]) {
  case ((chr, count) :: tail, c) if chr == c => (chr, count+1) :: tail
  case (xs, c) => (c, 1) :: xs
}
def say(c: Char, i: Int) = s"$i$c"

def lookAndSay2(s: String) = {
  val t = look(s).reverse.map((say _).tupled).mkString
  println(t.length)
  t
}

def dots(s: String): String = {
  val t = s + "."
  t
}

@tailrec
def repeat[T](n: Int, input: T)(f: T => T): T = {
  if (n == 0) {
    input
  } else {
    repeat(n-1, f(input))(f)
  }
}


val test = List(
  "1",
  "11",
  "21",
  "1211",
  "111221"
)

val conway = 1.303577269034

repeat(5, "x")(dots)
println()

repeat(5, "1")(lookAndSay2)
println()

//repeat[Int](10, 329356)(x => (x * conway).toInt)

repeat(50, "3113322113")(lookAndSay2)
