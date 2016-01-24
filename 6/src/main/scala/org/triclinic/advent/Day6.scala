package org.triclinic.advent

import java.awt._

import scala.io.Source

/**
 * Hello world!
 *
 */
object Day6 extends App {
  readInput()
  //drawArray()

  def readInput(): Unit = {
    for (line <- Source.fromURL(getClass.getResource("/input.txt")).getLines) {
      println(line)
    }
  }

  def drawArray():Unit = {
    val r = scala.util.Random
    val frame = new Frame {
      var painters = scala.List[Graphics => Unit]()
      def draw(f: Graphics => Unit) = { painters = f :: painters; repaint; }
      override def paint(g: Graphics) = {
        for (i <- 0 until 1000) {
          for (j <- 0 until 1000) {
            //if (r.nextInt(100) >= 50)
            //    g.drawLine(i,j,i,j)
            if (i > 100 && i < 200 && j > 400 && j < 500)
              g.drawLine(i,j,i,j)
          }
        }
      }

      this.setSize(1000,1000)
      this.show
    }
  }
}
