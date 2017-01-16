import scala.io.Source

/**
  * Created by mzimmerman on 1/16/17.
  */


def calc(input: Iterable[String]) = {
  var totalpaper = 0
  var totalribbon = 0
  for (line <- input) {
    val dim = line.split("x").take(3).map(_.toInt).sorted
    val area = Seq(dim(0) * dim(1), dim(1) * dim(2), dim(2) * dim(0)).sorted
    val paper = 2*area(0) + 2*area(1) + 2*area(2) + area(0)
    totalpaper += paper

    val vol = dim(0) * dim(1) * dim(2)
    val ribbon = 2*dim(0) + 2*dim(1) + vol
    totalribbon += ribbon
  }

  println(s"total paper = $totalpaper")
  println(s"total ribbon = $totalribbon")
}

val input = Source.fromFile("input2.txt").getLines().toList
calc(input)
