package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    for {
      (k, v) <- namedExpressions
    } yield (k, Signal(eval(v(), namedExpressions, Set(k))))

  def eval(expr: Expr, references: Map[String, Signal[Expr]], varVisited: Set[String])(using Signal.Caller): Double =
    expr match {
      case Expr.Literal(x) => x
      case Expr.Plus(x, y) => eval(x, references, varVisited) + eval(y, references, varVisited)
      case Expr.Minus(x, y) => eval(x, references, varVisited) - eval(y, references, varVisited)
      case Expr.Times(x, y) => eval(x, references, varVisited) * eval(y, references, varVisited)
      case Divide(x, y) => eval(x, references, varVisited) / eval(y, references, varVisited)
      case Expr.Ref(x) => {
        if varVisited.contains(x) then eval(Literal(Double.NaN), references, varVisited + x)
        else eval(getReferenceExpr(x, references), references, varVisited + x)
      }
    }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
