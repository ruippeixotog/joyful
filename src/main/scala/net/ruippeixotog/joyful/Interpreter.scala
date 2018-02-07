package net.ruippeixotog.joyful

import net.ruippeixotog.joyful.lang.Ast._
import net.ruippeixotog.joyful.lang.Parser
import net.ruippeixotog.joyful.lib.Primitives

object Interpreter {
  lazy val initialState = run(Parser.parseResource("lib/inilib.joy"), State(Nil, Map(), Primitives.registry))

  def run(prog: Program, initState: State = initialState): State = prog.steps.foldLeft(initState) {
    case (state, ModuleDef(cdef)) => state.copy(st = state.st ++ SymbolTable(cdef))
    case (state, EvalExpr(e)) => runStep(e, state)
  }

  def runStep(step: Term, st: State): State = step.factors.foldLeft(st) {
    case (state, Ident(name)) => (state.st.get(name), st.prims.get(name)) match {
      case (Some(ScopedTerm(dfn, locals)), _) => runStep(dfn, state.copy(st = state.st ++ locals)).copy(st = state.st)
      case (None, Some(f)) => f(state)
      case _ => throw new Exception(s"Unknown identifier: $name")
    }
    case (state, factor) => state.copy(stack = Value.fromFactor(factor) :: state.stack)
  }

  case class State(stack: List[Value], st: SymbolTable, prims: Map[String, State => State])

  type SymbolTable = Map[String, ScopedTerm]

  object SymbolTable {
    def apply(df: Definition): SymbolTable = df match {
      case SimpleDef(name, term) => Map(name -> ScopedTerm(term, Map()))
      case CompoundDef(_, pvt, pub) =>
        val pvtDef = pvt.flatMap(apply).toMap
        val pubDef = pub.flatMap(apply).toMap
        pubDef.mapValues { ctx => ctx.copy(locals = pvtDef ++ ctx.locals) }
    }
  }

  case class ScopedTerm(dfn: Term, locals: SymbolTable)
}
