package net.ruippeixotog.joyful.lib

import net.ruippeixotog.joyful.Interpreter.State
import net.ruippeixotog.joyful.Value

class LibBuilder {
  private[this] var _registry: Map[String, State => State] = Map()
  def registry = _registry

  protected[this] def prim(name: String)(f: State => State): Unit =
    _registry += (name -> f)

  protected[this] def primS(name: String)(f: PartialFunction[List[Value], List[Value]]): Unit =
    _registry += (name -> { st: State => st.copy(stack = f(st.stack)) })

  protected[this] def primEffS(name: String)(f: PartialFunction[List[Value], Unit]): Unit =
    _registry += (name -> { st: State => f(st.stack); st })

  protected[this] def primPush(name: String)(v: => Value): Unit =
    primS(name) { case rest => v :: rest }

  protected[this] def primTodo(name: String): Unit =
    primS(name) { case _ => throw new Exception(s"not implemented: $name") }
}
