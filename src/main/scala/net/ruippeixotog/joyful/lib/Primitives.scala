package net.ruippeixotog.joyful.lib

import scala.util.Random

import net.ruippeixotog.joyful.interpreter.Interpreter.ScopedTerm
import net.ruippeixotog.joyful.interpreter.Value._
import net.ruippeixotog.joyful.interpreter.{Interpreter, Value}
import net.ruippeixotog.joyful.lang.Parser

// http://www.kevinalbrecht.com/code/joy-mirror/html-manual.html
object Primitives extends LibBuilder {
  primPush("false")(false)
  primPush("true")(true)
  primPush("maxint")(Long.MaxValue)
  primPush("setsize")(65536)
  primS("stack") { case rest => rest.reverse :: rest }
  primTodo("conts")
  primTodo("autoput")
  primTodo("undeferror")
  primTodo("undefs")
  primTodo("echo")
  primTodo("clock")
  primPush("time")(System.currentTimeMillis() / 1000)
  primPush("rand")(Random.nextLong())
  primTodo("stdin")
  primTodo("stdout")
  primTodo("stderr")
  primS("id") { case rest => rest }
  primS("dup") { case x :: rest => x :: x :: rest }
  primS("swap") { case y :: x :: rest => x :: y :: rest }
  primS("rollup") { case z :: y :: x :: rest => y :: x :: z :: rest }
  primS("rolldown") { case z :: y :: x :: rest => x :: z :: y :: rest }
  primS("rotate") { case z :: y :: x :: rest => x :: y :: z :: rest }
  primS("popd") { case z :: _ :: rest => z :: rest }
  primS("dupd") { case z :: y :: rest => z :: y :: y :: rest }
  primS("swapd") { case z :: y :: x :: rest => z :: x :: y :: rest }
  primS("rollupd") { case w :: z :: y :: x :: rest => w :: y :: x :: z :: rest }
  primS("rolldownd") { case w :: z :: y :: x :: rest => w :: x :: z :: y :: rest }
  primS("rotated") { case w :: z :: y :: x :: rest => w :: x :: y :: z :: rest }
  primS("pop") { case _ :: rest => rest }
  primS("choice") {
    case _ :: t :: VBool(true) :: rest => t :: rest
    case f :: _ :: VBool(false) :: rest => f :: rest
  }
  primS("or") {
    case VBool(b2) :: VBool(b1) :: rest => (b2 || b1) :: rest
    case VSet(s2) :: VSet(s1) :: rest => (s2 ++ s1) :: rest
  }
  primS("xor") {
    case VBool(b2) :: VBool(b1) :: rest => (b2 ^ b1) :: rest
    case VSet(s2) :: VSet(s1) :: rest => (s2 ++ s1 -- s2.intersect(s1)) :: rest
  }
  primS("and") {
    case VBool(b2) :: VBool(b1) :: rest => (b2 && b1) :: rest
    case VSet(s2) :: VSet(s1) :: rest => s2.intersect(s1) :: rest
  }
  primS("not") {
    case VBool(b) :: rest => !b :: rest
    case VSet(s) :: rest => ((0 until 65536).toSet -- s) :: rest
  }
  primS("+") {
    case VInt(b) :: VInt(a) :: rest => (a + b) :: rest
    case VFloat(b) :: VFloat(a) :: rest => (a + b) :: rest
  }
  primS("-") {
    case VInt(b) :: VInt(a) :: rest => (a - b) :: rest
    case VFloat(b) :: VFloat(a) :: rest => (a - b) :: rest
  }
  primS("*") {
    case VInt(b) :: VInt(a) :: rest => (a * b) :: rest
    case VFloat(b) :: VFloat(a) :: rest => (a * b) :: rest
  }
  primS("/") {
    case VInt(b) :: VInt(a) :: rest => (a / b) :: rest
    case VFloat(b) :: VFloat(a) :: rest => (a / b) :: rest
  }
  primS("rem") {
    case VInt(b) :: VInt(a) :: rest => (a % b) :: rest
    case VFloat(b) :: VFloat(a) :: rest => (a % b) :: rest
  }
  primS("div") { case VInt(b) :: VInt(a) :: rest => (a % b) :: VInt(a / b) :: rest }
  primS("sign") {
    case VInt(a) :: rest => a.signum :: rest
    case VFloat(a) :: rest => a.signum.toDouble :: rest
  }
  primS("neg") {
    case VInt(a) :: rest => -a :: rest
    case VFloat(a) :: rest => -a :: rest
  }
  primS("ord") {
    case VChar(c) :: rest => c.toInt :: rest
    case VBool(b) :: rest => (if(b) 1 else 0) :: rest
    case VInt(n) :: rest => n :: rest
  }
  primS("chr") {
    case VInt(n) :: rest => n.toChar :: rest
    case VBool(b) :: rest => (if(b) 1 else 0).toChar :: rest
    case VChar(c) :: rest => c :: rest
  }
  primS("abs") {
    case VInt(a) :: rest => math.abs(a) :: rest
    case VFloat(a) :: rest => math.abs(a) :: rest
  }
  primS("acos") { case VFloat(a) :: rest => math.acos(a) :: rest }
  primS("asin") { case VFloat(a) :: rest => math.asin(a) :: rest }
  primS("atan") { case VFloat(a) :: rest => math.atan(a) :: rest }
  primS("atan2") { case VFloat(b) :: VFloat(a) :: rest => math.atan2(a, b) :: rest }
  primS("ceil") { case VFloat(a) :: rest => math.ceil(a) :: rest }
  primS("cos") { case VFloat(a) :: rest => math.cos(a) :: rest }
  primS("cosh") { case VFloat(a) :: rest => math.cosh(a) :: rest }
  primS("exp") { case VFloat(a) :: rest => math.exp(a) :: rest }
  primS("floor") { case VFloat(a) :: rest => math.floor(a) :: rest }
  primTodo("frexp")
  primS("ldexp") { case VInt(b) :: VFloat(a) :: rest => (a * math.pow(2, b)) :: rest }
  primS("log") { case VFloat(a) :: rest => math.log(a) :: rest }
  primS("log10") { case VFloat(a) :: rest => math.log10(a) :: rest }
  primTodo("modf")
  primS("pow") { case VFloat(b) :: VFloat(a) :: rest => math.pow(a, b) :: rest }
  primS("sin") { case VFloat(a) :: rest => math.sin(a) :: rest }
  primS("sinh") { case VFloat(a) :: rest => math.sinh(a) :: rest }
  primS("sqrt") { case VFloat(a) :: rest => math.sqrt(a) :: rest }
  primS("tan") { case VFloat(a) :: rest => math.tan(a) :: rest }
  primS("tanh") { case VFloat(a) :: rest => math.tanh(a) :: rest }
  primTodo("trunc")
  primTodo("localtime")
  primTodo("gmtime")
  primTodo("mktime")
  primTodo("strftime")
  primS("strtol") { case VInt(rdx) :: VStr(str) :: rest => java.lang.Integer.parseInt(str, rdx.toInt) :: rest }
  primS("strtod") { case VStr(str) :: rest => str.toDouble :: rest }
  primTodo("format")
  primTodo("formatf")
  primS("srand") { case VInt(n) :: rest => Random.setSeed(n); rest }
  primS("pred") { case VInt(n) :: rest => VInt(n - 1) :: rest }
  primS("succ") { case VInt(n) :: rest => VInt(n + 1) :: rest }
  primTodo("max")
  primTodo("min")
  primTodo("fclose")
  primTodo("feof")
  primTodo("ferror")
  primTodo("fflush")
  primTodo("fgetch")
  primTodo("fgets")
  primTodo("fopen")
  primTodo("fread")
  primTodo("fwrite")
  primTodo("fremove")
  primTodo("frename")
  primTodo("fput")
  primTodo("fputch")
  primTodo("fputchars")
  primTodo("fputstring")
  primTodo("fseek")
  primTodo("ftell")
  primTodo("unstack")
  primS("cons") {
    case VList(xs) :: x :: rest => VList(x :: xs) :: rest
    case VStr(str) :: VChar(ch) :: rest => VStr(ch +: str) :: rest
  }
  primS("swons") {
    case x :: VList(xs) :: rest => VList(x :: xs) :: rest
    case VChar(ch) :: VStr(str) :: rest => VStr(ch +: str) :: rest
  }
  primS("first") {
    case VList(xs) :: rest => xs.head :: rest
    case VStr(str) :: rest => VChar(str.head) :: rest
  }
  primS("rest") {
    case VList(xs) :: rest => VList(xs.tail) :: rest
    case VStr(str) :: rest => VStr(str.tail) :: rest
  }
  primTodo("compare")
  primTodo("at")
  primTodo("of")
  primS("size") {
    case VList(xs) :: rest => VInt(xs.length) :: rest
    case VStr(str) :: rest => VInt(str.length) :: rest
  }
  primTodo("opcase")
  primTodo("case")
  primS("uncons") {
    case VList(x :: xs) :: rest => VList(xs) :: x :: rest
    case VStr(str) :: rest => VStr(str.tail) :: VChar(str.head) :: rest
  }
  primS("unswons") {
    case VList(x :: xs) :: rest => x :: VList(xs) :: rest
    case VStr(str) :: rest => VChar(str.head) :: VStr(str.tail) :: rest
  }
  primTodo("drop")
  primTodo("take")
  primS("concat") {
    case VList(ys) :: VList(xs) :: rest => VList(xs ++ ys) :: rest
    case VStr(str2) :: VStr(str1) :: rest => VStr(str1 ++ str2) :: rest
  }
  primTodo("enconcat")
  primTodo("name")
  primS("intern") { case VStr(str) :: rest => VIdent(str) :: rest }
  prim("body") { st =>
    st.stack match {
      case VIdent(name) :: rest =>
        val nameTerm = st.termStack.head._2.get(name) match {
          case Some(ScopedTerm(dfn, _)) => Value.fromTerm(dfn)
          case _ => Nil
        }
        st.copy(stack = VList(nameTerm) :: rest)
    }
  }
  primS("null") {
    case VList(xs) :: rest => VBool(xs.isEmpty) :: rest
    case VStr(str) :: rest => VBool(str.isEmpty) :: rest
    case VInt(n) :: rest => VBool(n == 0) :: rest
    case VFloat(x) :: rest => VBool(x == 0.0) :: rest
  }
  primTodo("small")
  primTodo(">=")
  primS(">") {
    case VInt(b) :: VInt(a) :: rest => (a > b) :: rest
    case VFloat(b) :: VFloat(a) :: rest => (a > b) :: rest
    case VInt(b) :: VFloat(a) :: rest => (a > b) :: rest
    case VFloat(b) :: VInt(a) :: rest => (a > b) :: rest
    case VStr(str2) :: VStr(str1) :: rest => (str2 > str1) :: rest
    case VIdent(name2) :: VIdent(name1) :: rest => (name1 > name2) :: rest
  }
  primTodo("<=")
  primS("<") {
    case VInt(b) :: VInt(a) :: rest => (a < b) :: rest
    case VFloat(b) :: VFloat(a) :: rest => (a < b) :: rest
    case VInt(b) :: VFloat(a) :: rest => (a < b) :: rest
    case VFloat(b) :: VInt(a) :: rest => (a < b) :: rest
    case VStr(str2) :: VStr(str1) :: rest => (str2 < str1) :: rest
    case VIdent(name2) :: VIdent(name1) :: rest => (name1 < name2) :: rest
  }
  primTodo("!=")
  primS("=") {
    case VInt(b) :: VInt(a) :: rest => (a == b) :: rest
    case VFloat(b) :: VFloat(a) :: rest => (a == b) :: rest
    case VInt(b) :: VFloat(a) :: rest => (a == b) :: rest
    case VFloat(b) :: VInt(a) :: rest => (a == b) :: rest
    case VStr(str2) :: VStr(str1) :: rest => (str2 == str1) :: rest
    case VIdent(name2) :: VIdent(name1) :: rest => (name1 == name2) :: rest
  }
  primTodo("equal")
  primS("has") {
    case x :: VList(xs) :: rest => VBool(xs.contains(x)) :: rest
    case VChar(ch) :: VStr(str) :: rest => VBool(str.contains(ch)) :: rest
  }
  primS("in") {
    case VList(xs) :: x :: rest => VBool(xs.contains(x)) :: rest
    case VStr(str) :: VChar(ch) :: rest => VBool(str.contains(ch)) :: rest
  }
  primTodo("integer")
  primTodo("char")
  primTodo("logical")
  primTodo("set")
  primTodo("string")
  primS("list") {
    case (_: VList) :: rest => VBool(true) :: rest
    case _ :: rest => VBool(false) :: rest
  }
  primTodo("leaf")
  primTodo("user")
  primTodo("float")
  primTodo("file")

  prim("i") { st =>
    st.stack match {
      case VList(values) :: rest => st.copy(stack = rest, termStack = (values, st.termStack.head._2) :: st.termStack)
    }
  }
  prim("x") { st =>
    st.stack match {
      case VList(values) :: _ => st.copy(termStack = (values, st.termStack.head._2) :: st.termStack)
    }
  }
  prim("dip") { st =>
    st.stack match {
      case VList(values) :: x :: rest =>
        val resStack = Interpreter.runState(st.copy(stack = rest, termStack = (values, st.termStack.head._2) :: Nil)).stack
        st.copy(stack = x :: resStack)
    }
  }
  primTodo("app1")
  primTodo("app11")
  primTodo("app12")
  primTodo("construct")
  primTodo("nullary")
  primTodo("unary")
  primTodo("unary2")
  primTodo("unary3")
  primTodo("unary4")
  primTodo("app2")
  primTodo("app3")
  primTodo("app4")
  primTodo("binary")
  primTodo("ternary")
  primTodo("cleave")
  prim("branch") { st =>
    val (toExec, newStack) = st.stack match {
      case VList(fvs) :: _ :: VBool(false) :: rest => (fvs, rest)
      case _ :: VList(tvs) :: VBool(true) :: rest => (tvs, rest)
    }
    st.copy(stack = newStack, termStack = (toExec, st.termStack.head._2) :: st.termStack)
  }
  prim("ifte") { st =>
    st.stack match {
      case VList(fvs) :: VList(tvs) :: VList(cond) :: rest =>
        val condStack = Interpreter.runState(st.copy(stack = rest, termStack = (cond, st.termStack.head._2) :: Nil)).stack
        val toExec = condStack match {
          case VBool(false) :: _ => fvs
          case VBool(true) :: _ => tvs
        }
        st.copy(stack = rest, termStack = (toExec, st.termStack.head._2) :: st.termStack)
    }
  }
  primTodo("ifinteger")
  primTodo("ifchar")
  primTodo("iflogical")
  primTodo("ifset")
  primTodo("ifstring")
  primTodo("iflist")
  primTodo("iffloat")
  primTodo("iffile")
  prim("cond") { st =>
    st.stack match {
      case VList(values) :: rest =>
        val toExec = values.init.toStream.flatMap {
          case VList(VList(cond) :: vs) =>
            Interpreter.runState(st.copy(stack = rest, termStack = (cond, st.termStack.head._2) :: Nil)).stack match {
              case VBool(true) :: _ => Some(vs)
              case _ => None
            }
        }.headOption.getOrElse(values.last.asInstanceOf[VList].ls)

        st.copy(stack = rest, termStack = (toExec, st.termStack.head._2) :: st.termStack)
    }
  }
  primTodo("while")
  primTodo("linrec")
  primTodo("tailrec")
  primTodo("binrec")
  primTodo("genrec")
  primTodo("condnestrec")
  primTodo("condlinrec")
  primTodo("step")
  primTodo("fold")
  prim("map") { st =>
    st.stack match {
      case VList(f) :: VList(xs) :: rest =>
        val results = xs.map { x =>
          Interpreter.runState(st.copy(stack = x :: Nil, termStack = (f, st.termStack.head._2) :: Nil)).stack match {
            case res :: Nil => res
          }
        }
        st.copy(stack = VList(results) :: rest)

      case VList(f) :: VStr(str) :: rest =>
        val results = str.map { ch =>
          Interpreter.runState(st.copy(stack = VChar(ch) :: Nil, termStack = (f, st.termStack.head._2) :: Nil)).stack match {
            case VChar(res) :: Nil => res
          }
        }
        st.copy(stack = VStr(results) :: rest)
    }
  }
  prim("times") { st =>
    st.stack match {
      case VList(f) :: VInt(n) :: rest =>
        st.copy(stack = rest, termStack = List.fill(n.toInt)((f, st.termStack.head._2)) ++ st.termStack)
    }
  }
  prim("infra") { st =>
    st.stack match {
      case VList(p) :: VList(tmpStack) :: rest =>
        val resStack = Interpreter.runState(st.copy(stack = tmpStack, termStack = (p, st.termStack.head._2) :: Nil)).stack
        st.copy(stack = VList(resStack) :: rest)
    }
  }
  primTodo("primrec")
  primTodo("filter")
  primTodo("split")
  primTodo("some")
  primTodo("all")
  primTodo("treestep")
  primTodo("treerec")
  primTodo("treegenrec")

  primTodo("help")
  primTodo("helpdetail")
  primTodo("manual")

  primNoop("setautoput")
  primNoop("setundeferror")
  primNoop("setecho")

  primTodo("gc")
  primTodo("system")
  primTodo("getenv")
  primTodo("argv")
  primTodo("argc")

  primTodo("get")
  primS("put") { case x :: rest => /*print(x);*/ rest }
  primS("putch") {
    case VChar(n) :: rest => /*print(Char(n))*/ rest
    case VInt(n) :: rest => /*print(Char(n))*/ rest
  }
  primS("putchars") { case VStr(str) :: rest => /*print(str);*/ rest }
  prim("include") { st =>
    st.stack match {
      case VStr(filename) :: rest =>
        val file = Interpreter.run(Parser.parseResource(s"lib/$filename"))
        st.copy(stack = rest, globals = st.globals ++ file.globals)
    }
  }

  primTodo("abort")
  primTodo("quit")

  primNoop("__settracegc")
}
