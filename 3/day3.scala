import scala.collection.mutable.Map

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
  override def toString(): String = "(" + x + ", " + y + ")";

  def canEqual(other: Any): Boolean = other.isInstanceOf[Point]
}

object Point {
  def apply(x: Int, y: Int) = {
    new Point(x, y)
  }
}

var m = Map[Point, Integer]()

