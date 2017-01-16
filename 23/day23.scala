/**
  * Created by mzimmerman on 1/16/17.
  */

sealed trait Expr
case class Register(name: String) extends Expr
case class Literal(value: Int) extends Expr

sealed trait Instruction {
}
case class Half(r: Register) extends Instruction
case class Triple(r: Register) extends Instruction
case class Inc(r: Register) extends Instruction
case class Jump(r: Register, offset: Expr) extends Instruction
case class JumpIfEven(r: Register, offset: Expr) extends Instruction
case class JumpIfOdd(r: Register, offset: Expr) extends Instruction

object AsExpr {
  val IntString = """([-+]\d+)""".r

  def unapply(s: String) = s match {
    case IntString(n) => Some(Literal(n.toInt))
    case _ => Some(Register(s))
  }
}

val HalfString = """hlf (.+)""".r
val TripleString = """tpl (.+)""".r

def parse(code: Iterable[String]): Unit = {
  code.flatMap(_ match {
      case """hlf (.+)""" => Some(Half(Register(s)))
      case _ => None
  })
}

class Program(code: Iterable[String]) = {
  def parse(code: Iterable[String]): Unit = {

  }
}