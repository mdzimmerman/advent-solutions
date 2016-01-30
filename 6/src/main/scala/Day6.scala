import scala.io.Source
import java.awt._

class Day6 {
  val size = 1000
  val data = Array.ofDim[Boolean](size, size)

  def on(x1: Int, y1: Int, x2: Int, y2: Int) = 
    for (x <- x1 to x2;
         y <- y1 to y2)
      data(x)(y) = true

  def off(x1: Int, y1: Int, x2: Int, y2: Int) =
    for (x <- x1 to x2;
         y <- y1 to y2)
      data(x)(y) = false

  def toggle(x1: Int, y1: Int, x2: Int, y2: Int) =
    for (x <- x1 to x2;
         y <- y1 to y2)
      if (data(x)(y))
        data(x)(y) = false
      else
        data(x)(y) = true

  def printData() = 
    for (x <- 0 until size;
         y <- 0 until size) {
      val v = if (data(x)(y)) "#" else "."
      println(f"$x%3d $y%3d $v%s")
    }

  def parseInputFile(filename: String) {
    val source = Source.fromFile(filename)
    parseInputFile(source)
  }

  def parseInputFile(source: Source) {
    val Line = """(.+) (\d+),(\d+) through (\d+),(\d+)""".r

    for (line <- source.getLines) {
      line match {
        case Line("turn on",  x1, y1, x2, y2)
          => on(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
        case Line("turn off", x1, y1, x2, y2)
          => off(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
        case Line("toggle", x1, y1, x2, y2)
          => toggle(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
        case _ => println("NO MATCH")
      }
    }
  }

  def dataIter = {
    for (i <- 0 until size;
         j <- 0 until size)
      yield data(i)(j)
  }

  def countSet(): Int =
    dataIter.filter(_==true).length

  def countUnset(): Int =
    dataIter.filter(_==false).length
}

/*
val frame = new Frame {
  var painters = scala.List[Graphics => Unit]()
  def draw(f: Graphics => Unit) = { painters = f :: painters; repaint; }
  override def paint(g: Graphics) =
    for (i <- 0 until 1000)
      for (j <- 0 until 1000)
        if (data(i)(j))
          g.drawLine(i,j,i,j)
                
  this.setSize(1000,1000)
  this.show
}
*/

object Day6 extends App {
  override def main(args: Array[String]): Unit = {
    val d = new Day6
    if (args.length != 1)
      throw new IllegalArgumentException
    d.parseInputFile(args(0))
    println(d.countSet)
    println(d.countUnset)
  }
}
