import scala.collection.mutable
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
case class JumpIfOne(r: Register, offset: Expr) extends Instruction

object Instruction {
  val register     = """[ab]"""
  val literal      = """[-+]?\d+"""
  val reRegister   = s"""($register)""".r
  val reLiteral    = s"""($literal)""".r
  val reHalf       = s"""hlf ($register)""".r
  val reTriple     = s"""tpl ($register)""".r
  val reInc        = s"""inc ($register)""".r
  val reJump       = s"""jmp ($register|$literal)""".r
  val reJumpIfEven = s"""jie ($register), ($register|$literal)""".r
  val reJumpIfOne  = s"""jio ($register), ($register|$literal)""".r

  object AsRegister {
    def unapply(s: String): Option[Register] = s match {
      case reRegister(name) => Some(Register(name))
      case _ => None
    }
  }

  object AsExpr {
    def unapply(s: String): Option[Expr] = s match {
      case reRegister(name) => Some(Register(name))
      case reLiteral(value) => Some(Literal(value.toInt))
      case _ => None
    }
  }

  def parse(s: String): Option[Instruction] = s match {
    case reHalf(AsRegister(reg))   => Some(Half(reg))
    case reTriple(AsRegister(reg)) => Some(Triple(reg))
    case reInc(AsRegister(reg))    => Some(Inc(reg))
    case reJump(AsExpr(expr))      => Some(Jump(expr))
    case reJumpIfEven(AsRegister(reg), AsExpr(expr)) => Some(JumpIfEven(reg, expr))
    case reJumpIfOne(AsRegister(reg),  AsExpr(expr)) => Some(JumpIfOne(reg, expr))
    case _ => None
  }
}

case class Program(code: Array[Instruction]) {
  var length = code.length

  val register = new mutable.HashMap[String, Int]()
  var pointer = 0

  reset()

  def reset(a: Int = 0, b: Int = 0): Unit = {
    register("a") = a
    register("b") = b
    pointer = 0
  }

  private def getValue(expr: Expr): Int = {
    expr match {
      case Register(name) => register(name)
      case Literal(value) => value
    }
  }

  def evaluate(): Unit = {
    while (pointer >= 0 && pointer < length) {
      println(s"pos=$pointer a=${register("a")} b=${register("b")}")
      code(pointer) match {
        case Half(Register(name)) =>
          register(name) /= 2
          pointer += 1
        case Triple(Register(name)) =>
          register(name) *= 3
          pointer += 1
        case Inc(Register(name)) =>
          register(name) += 1
          pointer += 1
        case Jump(expr) =>
          pointer += getValue(expr)
        case JumpIfEven(Register(name), expr) =>
          if ((register(name) % 2) == 0)
            pointer += getValue(expr)
          else
            pointer += 1
        case JumpIfOne(Register(name), expr) =>
          if (register(name)  == 1)
            pointer += getValue(expr)
          else
            pointer += 1
      }
    }
    println(s"pos=$pointer a=${register("a")} b=${register("b")}")
  }
}

object Program {
  def apply(input: Iterator[String]): Program =
    new Program(input.flatMap(Instruction.parse).toArray)
}

val program = Program(Source.fromFile("input.txt").getLines)
var n = 0
for (i <- program.code) {
  println(s"$n $i")
  n += 1
}
program.evaluate()

program.reset(a=1, b=0)
program.evaluate()
//print(code.length)