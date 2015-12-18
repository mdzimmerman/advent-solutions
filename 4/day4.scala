import java.security.MessageDigest

val md5 = MessageDigest.getInstance("MD5")
val prefix = "iwrupvqb"
val stop = "000000"

var n: Integer = 0
var dig: String = ""
var str = prefix + n.toString()
do {
  n += 1
  if ( (n % 10000) == 0 ) println(n)
  str = prefix + n.toString
  dig = md5.digest(str.getBytes).map("%02X".format(_)).mkString
} while ( ! dig.startsWith(stop) )

println("%d %s %s".format(n, str, dig))
