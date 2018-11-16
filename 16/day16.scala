/**
  * Created by mzimmerman on 2/7/17.
  */

import scala.io.Source

val linePattern = """Sue (\d+): (.+)""".r
val pairPattern = """(.+): (\d+)""".r

case class Sue(n: Int, map: Map[String, Int]) {
  def matches(measured: Map[String, Int]): Boolean = {
    map.forall{case (k, v) => v == measured(k)}
  }

  def matches2(measured: Map[String, Int]): Boolean = {
    map.forall{case (k, v) => {
      k match {
        case "cats" | "trees" =>
          v > measured(k)
        case "pomeranians" | "goldfish" =>
          v < measured(k)
        case _ =>
          v == measured(k)
      }
    }}
  }
}

def parse(s: String) = {
  s match {
    case linePattern(n, text) =>
      val m = text.split(", ").flatMap({
        case pairPattern(k, v) => Some(k -> v.toInt)
        case _ => None
      }).toMap
      Some(Sue(n.toInt, m))
    case _ =>
      None
  }
}

val known = Map(
  "children" -> 3,
  "cats" -> 7,
  "samoyeds" -> 2,
  "pomeranians" -> 3,
  "akitas" -> 0,
  "vizslas" -> 0,
  "goldfish" -> 5,
  "trees" -> 3,
  "cars" -> 2,
  "perfumes" -> 1
)

//println(known)

for (line <- Source.fromFile("input.txt").getLines) {
  val sue = parse(line).get
  if (sue.matches(known))
    println(sue)
  if (sue.matches2(known))
    println(sue)
}

