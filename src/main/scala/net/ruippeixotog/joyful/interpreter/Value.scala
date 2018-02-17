package net.ruippeixotog.joyful.interpreter

import net.ruippeixotog.joyful.lang.Ast
import net.ruippeixotog.joyful.lang.Ast._

sealed trait Value

object Value {
  case class VBool(b: Boolean) extends Value {
    override def toString = b.toString
  }

  case class VChar(ch: Char) extends Value {
    override def toString = "'" + ch.toString
  }

  case class VInt(n: Long) extends Value {
    override def toString = n.toString
  }

  case class VSet(set: Set[Int]) extends Value {
    override def toString = set.mkString("{", " ", "}")
  }

  case class VStr(str: String) extends Value {
    override def toString = s""""$str""""
  }

  case class VList(ls: List[Value]) extends Value {
    override def toString = ls.mkString("[", " ", "]")
  }

  case class VFloat(x: Double) extends Value {
    override def toString = x.toString
  }

  case class VFile(x: Any) extends Value

  case class VIdent(name: String) extends Value {
    // require(name.headOption.exists(_.isDigit))
    override def toString = name
  }

  implicit def boolAsValue(b: Boolean) = VBool(b)
  implicit def charAsValue(ch: Char) = VChar(ch)
  implicit def intAsValue(n: Int) = VInt(n)
  implicit def longAsValue(n: Long) = VInt(n)
  implicit def setAsValue(set: Set[Int]) = VSet(set)
  implicit def listAsValue(ls: List[Value]) = VList(ls)
  implicit def doubleAsValue(x: Double) = VFloat(x)

  def fromTerm(term: Term): List[Value] =
    term.factors.map(fromFactor)

  def fromFactor(factor: Factor): Value = factor match {
    case Ast.FNumber(x) => if(x == x.toLong) VInt(x.toLong) else VFloat(x)
    case Ast.FChar(ch) => VChar(ch)
    case Ast.FStr(str) => VStr(str)
    case Ast.FSet(set) => VSet(set)
    case Ast.FQuoted(term) => VList(term.factors.map(fromFactor))
    case Ast.FIdent(name) => VIdent(name)
  }
}
