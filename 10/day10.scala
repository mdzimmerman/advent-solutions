import scala.annotation.tailrec

/**
  * Created by mzimmerman on 1/20/17.
  */

def lookAndSay(s: String): String = lookAndSay(s.tail, s.head, 1, "")

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

//def identity(s: String) = s

@tailrec
def repeat(n: Int, input: String)(f: String => String): String = {
  println(s"$n => ${input.length} => $input")
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

for (t <- test) println(s"$t -> ${lookAndSay(t)}")

repeat(5, "x")(x => x+".")

repeat(5, "1")(lookAndSay)

repeat(20, "3113322113")(lookAndSay)
//var s =
//for (n <- 1 to 40)