package net.ruippeixotog.joyful

import net.ruippeixotog.joyful.lang.Ast._

sealed trait Value
case class BoolVal(b: Boolean) extends Value
case class CharVal(ch: Char) extends Value
case class IntVal(n: Long) extends Value
case class SetVal(set: Set[Int]) extends Value
case class StrVal(str: String) extends Value
case class ListVal(ls: List[Value]) extends Value
case class FloatVal(x: Double) extends Value
case class FileVal(x: Any) extends Value
case class IdentVal(name: String) extends Value

object Value {
  def fromFactor(factor: Factor): Value = factor match {
    case Number(x) => if(x == x.toLong) IntVal(x.toLong) else FloatVal(x)
    case Character(ch) => CharVal(ch)
    case Text(str) => StrVal(str)
    case Seq(seq) => ListVal(seq.map(fromFactor))
    case Quoted(term) => ListVal(term.factors.map(fromFactor))
    case Ident(name) => IdentVal(name)
  }

  object Conversions {
    implicit def boolAsValue(b: Boolean) = BoolVal(b)
    implicit def charAsValue(ch: Char) = CharVal(ch)
    implicit def intAsValue(n: Int) = IntVal(n)
    implicit def longAsValue(n: Long) = IntVal(n)
    implicit def setAsValue(set: Set[Int]) = SetVal(set)
    implicit def listAsValue(ls: List[Value]) = ListVal(ls)
    implicit def doubleAsValue(x: Double) = FloatVal(x)
  }
}
