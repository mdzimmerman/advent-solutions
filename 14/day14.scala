import scala.io.Source

/**
  * Created by mzimmerman on 1/24/17.
  */


case class Reindeer(name: String, speed: Int, timeFlight: Int, timeRest: Int) {
  val timeCycle = timeFlight + timeRest
  var points = 0

  def distanceAtTime(time: Int): Int = {
    val nPriorCycles = time / timeCycle
    val distPrior = speed * timeFlight * nPriorCycles

    val timeCurrent = time % timeCycle
    val distCurrent = if ( timeCurrent > timeFlight ) timeFlight * speed else timeCurrent * speed

    distPrior + distCurrent
  }
}

case class Race(reindeer: List[Reindeer]) {
  def winner(time: Int) = {
    val w = reindeer.map(r => (r, r.distanceAtTime(time))).sortWith(_._2 > _._2)
    w.filter(_._2 == w.head._2).map(_._1)
  }
  def printDistanceByTime(maxTime: Int) = {
    print("%4s".format(" "))
    for (r <- reindeer) print(" %8s".format(r.name))
    println()
    for (t <- 0 to maxTime) {
      print("%4d".format(t))
      for (r <- reindeer) print( " %8d".format(r.distanceAtTime(t)))
      println()
    }
  }

  def winnerByPoints(maxTime: Int) = {
    for (r <- reindeer) r.points = 0

    for (t <- 1 to maxTime) {
      for (r <- winner(t)) r.points += 1
    }

    for (r <- reindeer) {
      println(s"${r.name} ${r.points}")
    }
  }
}

val ReindeerString = """(.+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

def parse(s: String): Option[Reindeer] = s match {
  case ReindeerString(name, speed, timeFlight, timeRest) =>
    Some(Reindeer(name, speed.toInt, timeFlight.toInt, timeRest.toInt))
  case _ =>
    None
}

def parseList(ls: List[String]) = Race(ls.flatMap(parse(_)))

val test = Race(List(
  Reindeer("Comet", 14, 10, 127),
  Reindeer("Dancer", 16, 11, 162)))
//test.printDistanceByTime(200)
println(test.winner(1000))
test.winnerByPoints(1000)

//println(test.winners(1000))

val input = parseList(Source.fromFile("input.txt").getLines.toList)
//input.printDistanceByTime(200)
input.winnerByPoints(2503)

//for (t <- 0 to 200) {
//  println("%3d %6d".format(t, comet.distanceAtTime(t)))
//}