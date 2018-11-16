import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class Subst(in: String, out: String) {
  val window = in.length

  def generate(s: String): List[String] = {
    val sNewAll = ArrayBuffer[String]()

    for (i <- 0 to s.length - window) {
      val bef = s.substring(0, i)
      val mid = s.substring(i, i + window)
      val aft = s.substring(i + window)
      //println(s"[$bef][$mid][$aft]")
      if (mid == in) {
        val sNew = bef + out + aft
        sNewAll += sNew
      }
    }
    sNewAll.toList
  }
}

object Subst {
  val SubstPattern = "(.+) => (.+)".r

  def parse(s: String): Option[Subst] = s match {
    case SubstPattern(in, out) => Some(Subst(in, out))
    case _ => None
  }
}

def generateAll(in: String, subs: List[Subst]): Set[String] = {
  val out = Set[String]()

  for (s <- subs)
    s.generate(in).foreach(out.add)
  out
}

val s = List(
  "H => OH",
  "H => HO",
  "O => HH").flatMap(Subst.parse)

println(generateAll("HOOH", s).size)
println(generateAll("HOHOHO", s).size)

val s2 = Source.fromFile("input.txt").getLines.flatMap(Subst.parse).toList
println(s2)

val text = Source.fromFile("input.txt").getLines.toList.tail
println(text)