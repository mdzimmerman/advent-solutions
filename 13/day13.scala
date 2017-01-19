import com.sun.prism.shader.DrawCircle_LinearGradient_PAD_AlphaTest_Loader

import scala.io.Source

/**
  * Created by mzimmerman on 1/18/17.
  */

class Circle(lines: List[String]) {
  val names = lines.map(_.split(" ").head).distinct.toSeq ++ Seq("Me")
  val nameMap = names.zipWithIndex.map{case(n, i) => n -> i}.toMap

  val happiness = Array.ofDim[Int](names.length+1, names.length+1)

  val inputPat = """(.+) would (lose|gain) (\d+) happiness units by sitting next to (.+)\.""".r

  lines.foreach {
    case inputPat(name1, dir, nStr, name2) =>
      println(s"name1=$name1 dir=$dir n=$nStr name2=$name2")
      val n = if (dir == "lose") -nStr.toInt else nStr.toInt
      //val (i, j) = if (nameMap(name1) > nameMap(name2)) (nameMap(name2), nameMap(name1)) else (nameMap(name1), nameMap(name2))
      val (i, j) = (nameMap(name1), nameMap(name2))
      happiness(i)(j) += n
    case _ => // do nothing
  }

  def printGrid() = {
    print("    ")
    for (j <- names.indices) {
      print(" %3d".format(j))
    }
    println()
    for (i <- names.indices) {
      print(" %3d".format(i))
      for (j <- names.indices) {
        print(" %3d".format(happiness(i)(j)))
      }
      println()
    }
  }

  def calcForOrder(order: Seq[Int]): Int = {
    val sum = order.sliding(2).map(s => {
      happiness(s(0))(s(1)) + happiness(s(1))(s(0))
    }).sum
    val ends = happiness(order.last)(order.head) + happiness(order.head)(order.last)
    sum + ends
  }

  def calcMax(): Int = {
    var max = 0
    for (p <- names.indices.permutations) {
      val s = calcForOrder(p)
      println(s"${p.mkString("-")} => $s")
      if (s > max) max = s
    }
    max
  }
}

val test = new Circle(Source.fromFile("test.txt").getLines().toList)
test.printGrid()
println(test.calcMax())

val input = new Circle(Source.fromFile("input.txt").getLines().toList)
input.printGrid()
println(input.calcMax())