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
  case class Ident(name: String) extends Factor
  case class Number(x: Double) extends Factor
  case class Character(ch: Char) extends Factor
  case class Text(str: String) extends Factor
  case class Seq(seq: List[Factor]) extends Factor
  case class Quoted(term: Term) extends Factor
}
