import scala.io.Source

/**
  * Created by mzimmerman on 1/16/17.
  */
sealed trait Expr
case class Register(name: String) extends Expr
case class Literal(value: Int) extends Expr

sealed trait Instruction
case class Half(r: Register) extends Instruction
case class Triple(r: Register) extends Instruction
case class Inc(r: Register) extends Instruction
case class Jump(offset: Expr) extends Instruction
case class JumpIfEven(r: Register, offset: Expr) extends Instruction
case class JumpIfOdd(r: Register, offset: Expr) extends Instruction

object Instruction {
  val RegisterMatch = """([ab])""".r
  val LiteralMatch = """([-+]?\d+)""".r

  object AsRegister {
    def unapply(s: String): Option[Register] = s match {
      case RegisterMatch(name) => Some(Register(name))
      case _ => None
    }
  }

  object AsExpr {
    def unapply(s: String): Option[Expr] = s match {
      case RegisterMatch(name) => Some(Register(name))
      case LiteralMatch(value) => Some(Literal(value.toInt))
      case _ => None
    }
  }

  def parse(s: String): Option[Instruction] = s match {
    case "hlf (.+)".r.(AsRegister(name)) => Some(Half(name))
    case List("tpl", AsRegister(name)) => Some(Triple(name))
    case List("inc", AsRegister(name)) => Some(Inc(name))
    case List("jmp", AsExpr(expr)) => Some(Jump(expr))
    case List("jie", AsRegister(name), AsExpr(expr)) => Some(JumpIfEven(name, expr))
    case List("jio", AsRegister(name), AsExpr(expr)) => Some(JumpIfOdd(name, expr))
    case _ => None
  }
}

val code = Source.fromFile("input.txt").getLines.flatMap(Instruction.parse(_))
for (i <- code)
  println(i)
