/**
  * Created by mzimmerman on 2/7/17.
  */

val linePattern = """Sue (\d+): (.+)""".r
val pairPattern = """(.+)"""

def parse(s: String) = {
  s match {
    case linePattern(n, text) =>
      val m = text.split(", ").flatMap({
        case ""
      })
    case _ => None
  }
}