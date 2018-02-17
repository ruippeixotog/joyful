package net.ruippeixotog.joyful.lang

object Ast {
  case class Program(steps: List[ProgramStep])

  sealed trait ProgramStep
  case class ModuleDef(cdef: CompoundDef) extends ProgramStep
  case class EvalExpr(term: Term) extends ProgramStep

  sealed trait Definition
  case class SimpleDef(name: String, value: Term) extends Definition
  case class CompoundDef(module: Option[String], pvt: List[Definition], pub: List[Definition]) extends Definition

  case class Term(factors: List[Factor])

  sealed trait Factor
  case class FIdent(name: String) extends Factor
  case class FNumber(x: Double) extends Factor
  case class FChar(ch: Char) extends Factor
  case class FStr(str: String) extends Factor
  case class FSet(seq: Set[Int]) extends Factor
  case class FQuoted(term: Term) extends Factor
}
