import scala.io.Source

/**
  * Created by mzimmerman on 2/1/17.
  */

val numberPattern = """-?[0-9]+""".r

def sum(s: String) = {
  numberPattern.findAllIn(s).map(_.toInt).sum
}

val test = List(
  """[1,2,3]""",
  """{"a":2,"b":4}""",
  """[[[3]]]""",
  """{"a":{"b":4},"c":-1}""",
  """{"a":[-1,1]}""",
  """[-1,{"a":1}]""",
  """[]""",
  """{}""")
val input = Source.fromFile("input.txt").getLines()

println("--- test ---")
for (t <- test) {
  println(s"$t -> ${sum(t)}")
}
println()

println("--- input ---")
println(s"part #1 = ${input.map(sum).sum}")