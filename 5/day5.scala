/**
  * Created by mzimmerman on 12/18/15.
  */

import scala.io.Source

val filename = "input.txt"
for (line <- Source.fromFile(filename).getLines) {
  println(line)
}