import scala.collection.mutable.Map
import scala.io.Source

/**
  * Created by mzimmerman on 11/5/15.
  */
class Point(val x: Int, val y: Int) {
  override def hashCode = 41 * (41 + x) + y
  override def equals(other: Any) = other match {
    case that: Point =>
      (that canEqual this) && (this.x == that.x) && (this.y == that.y)
    case _ =>
      false
  }
  override def toString(): String = "(" + x + ", " + y + ")"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

  def nextPoint(direction: Char): Point = {
    direction match {
      case '^' => new Point(x, y+1)
      case '>' => new Point(x+1, y)
      case 'v' => new Point(x, y-1)
      case '<' => new Point(x-1, y)
      case _ => new Point(x, y)
    }
  }
}

object Point {
  def apply(x: Int, y: Int) = {
    new Point(x, y)
  }
}

def updateMap(m: Map[Point,Integer], p: Point): Map[Point,Integer] = {
  m.get(p) match {
    case Some(c) => m + (p -> (c+1))
    case None => m + (p -> 1)
  }
}

var m = Map[Point, Integer]()
var p1 = Point(0, 0)
var p2 = Point(0, 0)
val filename = "input.txt"
var i = 1
for (line <- Source.fromFile(filename).getLines) {
  for (c <- line) {
    if ((i % 2) == 1) {
      m = updateMap(m, p1)
      println(i.toString + " p1 " + p1.toString + " " + c)
      p1 = p1.nextPoint(c)
    }
    else {
      m = updateMap(m, p2)
      println(i.toString + " p2 " + p2.toString + " " + c)
      p2 = p2.nextPoint(c)
    }
    i += 1
  }
}
if ((i % 2) == 1) {
  m = updateMap(m, p1)
  println(i.toString + " p1 " + p1.toString + " ")
}
else {
  m = updateMap(m, p2)
  println(i.toString + " p2 " + p2.toString + " ")
}

val xmin = m.keys.map(_.x).min
val xmax = m.keys.map(_.x).max
val ymin = m.keys.map(_.y).min
val ymax = m.keys.map(_.y).max

for (y <- ymax until ymin-1 by -1) {
  for (x <- xmin until xmax+1) {
    m.get(Point(x,y)) match {
      case Some(c) => print("#")
      case None => if ( x == 0 ) print("|") else if ( y == 0 ) print("-") else print(".")
    }
  }
  println("")
}

println("count: " + m.keys.toList.length.toString)

//m.foreach{
//  case(p, n) => println(p.toString + ": " + n.toString)
//}