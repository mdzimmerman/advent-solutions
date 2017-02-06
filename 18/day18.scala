import scala.annotation.tailrec
import scala.io.Source
import scala.math.sqrt

/**
  * Created by mzimmerman on 1/27/17.
  */

case class LightGrid(grid: Vector[Boolean]) {
  val size = sqrt(grid.length).toInt

  def get(x: Int, y: Int) = if (x >= 0 && x < size && y >= 0 && y < size) {
    if ( inCorner(x, y) ) {
      true
    } else {
      grid(y * size + x)
    }
  } else {
    false
  }


  def inCorner(x: Int, y: Int): Boolean = {
    if ((x == 0 || x == size-1) && (y == 0 || y == size-1))
      true
    else
      false
  }

  def neighbors(x: Int, y: Int) = {
    val dirs = List(
      (x-1, y-1),
      (x,   y-1),
      (x+1, y-1),
      (x-1, y  ),
      (x+1, y  ),
      (x-1, y+1),
      (x,   y+1),
      (x+1, y+1))

    for (d <- dirs) yield get(d._1, d._2)
  }

  def countNeighbors(x: Int, y: Int): Int = {
    neighbors(x, y).count(_ == true)
  }

  def cycle(): LightGrid = {
    val newGrid = grid.zipWithIndex.map{case(cell, index) =>
      val x = index % size
      val y = index / size
      val n = countNeighbors(x, y)
      if ( inCorner(x,y) ) {
        true
      } else {
        if (cell == true) {
          if (n == 2 || n == 3) true else false
        } else {
          if (n == 3) true else false
        }
      }
    }
    LightGrid(newGrid)
  }

  def count(): Int = {
    grid.count(_ == true)
  }

  def printGrid(): Unit = {
    for (y <- 0 until size) {
      for (x <- 0 until size) {
        print(if (get(x, y)) "#" else ".")
      }
      println()
    }
  }
}

def parse(input: Source): LightGrid = {
  val grid = input.flatMap{
    case '#' => Some(true)
    case '.' => Some(false)
    case _ => None
  }.toVector
  LightGrid(grid)
}

@tailrec
def loop(grid: LightGrid, n: Int): LightGrid = {
  //grid.printGrid()
  //println()
  if ( n == 0 )
    grid
  else
    loop(grid.cycle(), n-1)
}

val test = parse(Source.fromFile("test.txt"))

loop(test, 5)

val input = parse(Source.fromFile("input.txt"))
val output = loop(input, 100)
output.printGrid()
println(output.count)