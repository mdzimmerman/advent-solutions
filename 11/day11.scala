val test = "ghijklmn"

def incr(c: Char): Char = {
    if (c == 'z')
        'a'
    else
        (c+1).toChar
}

def incrString(s: String): String = incrStringList(s.reverse.toList).reverse.mkString("")

def incrStringList(l: List[Char]): List[Char] = {
  val lh = incr(l.head)
  if (lh == 'a')
     lh :: incrStringList(l.tail)
  else if (lh == 'i' || lh == 'l' || lh == 'o')
    incr(lh) :: l.tail
  else
    lh :: l.tail
}

def check(s: String): Boolean = {
  val sq = s.toSeq
  checkWhitelist(sq) && checkDoublet(sq) && checkTriplet(sq)
}

def checkWhitelist(s: Seq[Char]): Boolean = {
  !(s.contains('i') || s.contains('l') || s.contains('o'))
}

def checkDoublet(s: Seq[Char]): Boolean = {
  val len = s.length
  for (i <- 0 until len-3)
    if (s(i) == s(i+1))
      for (j <- i+2 until len-1)
        if (s(j) == s(j+1))
          if (s(i) != s(j))
            return true
  return false
}

def checkTriplet(s: Seq[Char]): Boolean = {
  s.sliding(3).exists { w =>
    w(0) <= 'x' && incr(w(0)) == w(1) && incr(w(1)) == w(2)
  }
}

def iterateUntilFound(s: String): String = {
  var t = s
  while (!check(t)) {
    //println(s"$t == ${check(t)}")
    t = incrString(t)
  }
  t
}

val i = "cqjxjnds"
val i2 = iterateUntilFound(i)
println(s"part #1 = $i2")

val i3 = iterateUntilFound(incrString(i2))
println(s"part #2 = $i3")
