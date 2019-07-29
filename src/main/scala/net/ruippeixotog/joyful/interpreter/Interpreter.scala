package net.ruippeixotog.joyful.interpreter

import net.ruippeixotog.joyful.lang.Ast._
import net.ruippeixotog.joyful.lang.Parser
import net.ruippeixotog.joyful.lib.Primitives

object Interpreter {
  lazy val initialState = run(Parser.parseResource("lib/inilib.joy"), State(Nil, Nil, Map(), Primitives.registry))

  def run(prog: Program, initState: State = initialState): State = prog.steps.foldLeft(initState) {
    case (state, ModuleDef(cdef)) => state.copy(globals = state.globals ++ SymbolTable(cdef))
    case (state, EvalExpr(term)) => runState(state.copy(termStack = (Value.fromTerm(term), state.globals) :: Nil))
  }

  def runState(state: State): State = state.termStack match {
    case Nil => state
    case (Nil, _) :: terms => runState(state.copy(termStack = terms))
    case (value :: values, symbols) :: terms => runState {
      value match {
        case Value.VIdent(name) => (symbols.get(name), state.prims.get(name)) match {
          case (Some(ScopedTerm(dfn, locals)), _) =>
            state.copy(termStack = (Value.fromTerm(dfn), symbols ++ locals) :: (values, symbols) :: terms)
          case (None, Some(f)) =>
            try {
              f(state.copy(termStack = (values, symbols) :: terms))
            } catch {
              case ex: Exception =>
                println(s"Exception thrown at primitive $name (${ex.getClass.getName})")
                println(s"Stack (size ${state.stack.length}, top 3):")
                state.stack.take(3).map(_.toString.filterNot(_ == '\n').filterNot(_ == '\t')).foreach(println)
                throw ex
            }
          case _ => throw new Exception(s"Unknown identifier: $name")
        }
        case _ => state.copy(
          stack = value :: state.stack,
          termStack = (values, symbols) :: terms)
      }
    }
  }

  case class State(stack: List[Value],
                   termStack: List[(List[Value], SymbolTable)],
                   globals: SymbolTable,
                   prims: Map[String, State => State])

  type SymbolTable = Map[String, ScopedTerm]

  object SymbolTable {
    def apply(df: Definition): SymbolTable = df match {
      case SimpleDef(name, term) => Map(name -> ScopedTerm(term, Map()))
      case CompoundDef(_, pvt, pub) =>
        val pvtDef = pvt.flatMap(apply).toMap
        val pubDef = pub.flatMap(apply).toMap
        pubDef.view.mapValues { ctx => ctx.copy(locals = pvtDef ++ ctx.locals) }.toMap
    }
  }

  case class ScopedTerm(dfn: Term, locals: SymbolTable)
}
