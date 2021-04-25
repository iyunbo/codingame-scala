package org.iyunbo.coding
package frp

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map { case (name: String, expr: Signal[Expr]) => (name, Signal(eval(expr(), namedExpressions))) }
  }

  def isCyclic(usedNames: List[Ref], expr: Expr, references: Map[String, Signal[Expr]]): Boolean = {
    expr match {
      case _: Literal => false
      case ref: Ref => usedNames.contains(ref) || isCyclic(ref :: usedNames, getReferenceExpr(ref.name, references), references)
      case pls: Plus => isCyclic(usedNames, pls.a, references) || isCyclic(usedNames, pls.b, references)
      case mns: Minus => isCyclic(usedNames, mns.a, references) || isCyclic(usedNames, mns.b, references)
      case tms: Times => isCyclic(usedNames, tms.a, references) || isCyclic(usedNames, tms.b, references)
      case dvd: Divide => isCyclic(usedNames, dvd.a, references) || isCyclic(usedNames, dvd.b, references)
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case lit: Literal => lit.v
      case ref: Ref =>
        val newExp: Expr = getReferenceExpr(ref.name, references)
        if (isCyclic(List(ref), newExp, references)) {
          Double.NaN
        } else {
          eval(newExp, references)
        }
      case pls: Plus => eval(pls.a, references) + eval(pls.b, references)
      case mns: Minus => eval(mns.a, references) - eval(mns.b, references)
      case tms: Times => eval(tms.a, references) * eval(tms.b, references)
      case dvd: Divide => eval(dvd.a, references) / eval(dvd.b, references)
    }
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
