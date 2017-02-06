import scala.io.Source

/**
  * Created by mzimmerman on 1/24/17.
  */

case class Container(volume: Int, index: Int)

def buildSet(xs: List[Int]): Set[Container] =
  xs.zipWithIndex.map{case(v,i) => Container(v, i)}.toSet

def countSubsets(containers: Set[Container], targetVol: Int) = {
  var count = 0
  for (s <- containers.subsets) {
    val sl = s.toList.map(_.volume)
    val v = sl.sum
    //println(s"$s => $v")
    if (v == targetVol) {
      println(s"$sl => $v")
      count += 1
    }
  }
  println(count)
}

def countSubsets(containers: Set[Container], targetVol: Int, setSize: Int) = {
  var count = 0
  for (s <- containers.subsets) {
    val sl = s.toList.map(_.volume)
    val v = sl.sum
    //println(s"$s => $v")
    if (v == targetVol && setSize == sl.length) {
      println(s"$sl => $v")
      count += 1
    }
  }
  println(count)
}

val test = buildSet(List(20, 15, 10, 5, 5))
countSubsets(test, 25)

val input = buildSet(Source.fromFile("input.txt").getLines.map(_.toInt).toList)
countSubsets(input, 150)

countSubsets(input, 150, 4)