import scala.io.Source
import java.awt._

class BoolArray {
  val size = 1000
  val data = Array.ofDim[Boolean](size, size)

  def on(x1: Int, y1: Int, x2: Int, y2: Int) =
    for (x <- x1 to x2; y <- y1 to y2)
      data(x)(y) = true

  def off(x1: Int, y1: Int, x2: Int, y2: Int) =
    for (x <- x1 to x2; y <- y1 to y2)
      data(x)(y) = false

  def toggle(x1: Int, y1: Int, x2: Int, y2: Int) =
    for (x <- x1 to x2; y <- y1 to y2)
      data(x)(y) = !data(x)(y)

  def printData() =
    for (x <- 0 until size; y <- 0 until size) {
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
        case Line("turn on", x1, y1, x2, y2)
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
    for (i <- 0 until size; j <- 0 until size)
      yield data(i)(j)
  }

  def countSet(): Int =
    dataIter.filter(_ == true).length

  def countUnset(): Int =
    dataIter.filter(_ == false).length

  def drawImage(): Unit = {
    val windowSize = size
    val frame = new Frame {
      var painters = scala.List[Graphics => Unit]()

      def draw(f: Graphics => Unit) = {
        painters = f :: painters; repaint;
      }

      override def paint(g: Graphics) =
        for (i <- 0 until windowSize; j <- 0 until windowSize)
          if (data(i)(j))
            g.drawLine(i, j, i, j)

      this.setSize(windowSize, windowSize)
    }
    frame.show()
  }
}

class IntArray {
  val size = 1000
  val data = Array.ofDim[Int](size, size)
  rangeApply(0, size-1, 0, size-1, (i) => 0)

  def rangeApply(x1: Int, y1: Int, x2: Int, y2: Int, f: Int => Int) =
    for (x <- x1 to x2; y <- y1 to y2)
      data(x)(y) = f( data(x)(y) )

  def iter() =
    for (i <- 0 until size; j <- 0 until size)
      yield data(i)(j)

  def on(x1: Int, y1: Int, x2: Int, y2: Int) =
    rangeApply(x1, y1, x2, y2, (i) => i+1)

  def off(x1: Int, y1: Int, x2: Int, y2: Int) =
    rangeApply(x1, y1, x2, y2, (i) => if (i>0) i-1 else 0)

  def toggle(x1: Int, y1: Int, x2: Int, y2: Int) =
    rangeApply(x1, y1, x2, y2, (i) => i+2)

  def sum(): Int = iter.sum

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
}

object Day6 extends App {
  override def main(args: Array[String]): Unit = {
    val d = new IntArray
    println(d.sum)
    if (args.length != 1)
      throw new IllegalArgumentException
    d.parseInputFile(args(0))
    println(d.sum)
  }
}
