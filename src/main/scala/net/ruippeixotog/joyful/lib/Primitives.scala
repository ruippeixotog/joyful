package net.ruippeixotog.joyful.lib

import scala.util.Random

import net.ruippeixotog.joyful.Value.Conversions._
import net.ruippeixotog.joyful._

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
    case _ :: t :: BoolVal(true) :: rest => t :: rest
    case f :: _ :: BoolVal(false) :: rest => f :: rest
  }
  primS("or") {
    case BoolVal(b2) :: BoolVal(b1) :: rest => (b2 || b1) :: rest
    case SetVal(s2) :: SetVal(s1) :: rest => (s2 ++ s1) :: rest
  }
  primS("xor") {
    case BoolVal(b2) :: BoolVal(b1) :: rest => (b2 ^ b1) :: rest
    case SetVal(s2) :: SetVal(s1) :: rest => (s2 ++ s1 -- s2.intersect(s1)) :: rest
  }
  primS("and") {
    case BoolVal(b2) :: BoolVal(b1) :: rest => (b2 && b1) :: rest
    case SetVal(s2) :: SetVal(s1) :: rest => s2.intersect(s1) :: rest
  }
  primS("not") {
    case BoolVal(b) :: rest => !b :: rest
    case SetVal(s) :: rest => ((0 until 65536).toSet -- s) :: rest
  }
  primS("+") {
    case IntVal(b) :: IntVal(a) :: rest => (a + b) :: rest
    case FloatVal(b) :: FloatVal(a) :: rest => (a + b) :: rest
  }
  primS("-") {
    case IntVal(b) :: IntVal(a) :: rest => (a - b) :: rest
    case FloatVal(b) :: FloatVal(a) :: rest => (a - b) :: rest
  }
  primS("*") {
    case IntVal(b) :: IntVal(a) :: rest => (a * b) :: rest
    case FloatVal(b) :: FloatVal(a) :: rest => (a * b) :: rest
  }
  primS("/") {
    case IntVal(b) :: IntVal(a) :: rest => (a / b) :: rest
    case FloatVal(b) :: FloatVal(a) :: rest => (a / b) :: rest
  }
  primS("rem") {
    case IntVal(b) :: IntVal(a) :: rest => (a % b) :: rest
    case FloatVal(b) :: FloatVal(a) :: rest => (a % b) :: rest
  }
  primS("div") { case IntVal(b) :: IntVal(a) :: rest => (a % b) :: IntVal(a / b) :: rest }
  primS("sign") {
    case IntVal(a) :: rest => a.signum :: rest
    case FloatVal(a) :: rest => a.signum.toDouble :: rest
  }
  primS("neg") {
    case IntVal(a) :: rest => -a :: rest
    case FloatVal(a) :: rest => -a :: rest
  }
  primS("ord") {
    case CharVal(c) :: rest => c.toInt :: rest
    case BoolVal(b) :: rest => (if(b) 1 else 0) :: rest
    case IntVal(n) :: rest => n :: rest
  }
  primS("chr") {
    case IntVal(n) :: rest => n.toChar :: rest
    case BoolVal(b) :: rest => (if(b) 1 else 0).toChar :: rest
    case CharVal(c) :: rest => c :: rest
  }
  primS("abs") {
    case IntVal(a) :: rest => math.abs(a) :: rest
    case FloatVal(a) :: rest => math.abs(a) :: rest
  }
  primS("acos") { case FloatVal(a) :: rest => math.acos(a) :: rest }
  primS("asin") { case FloatVal(a) :: rest => math.asin(a) :: rest }
  primS("atan") { case FloatVal(a) :: rest => math.atan(a) :: rest }
  primS("atan2") { case FloatVal(b) :: FloatVal(a) :: rest => math.atan2(a, b) :: rest }
  primS("ceil") { case FloatVal(a) :: rest => math.ceil(a) :: rest }
  primS("cos") { case FloatVal(a) :: rest => math.cos(a) :: rest }
  primS("cosh") { case FloatVal(a) :: rest => math.cosh(a) :: rest }
  primS("exp") { case FloatVal(a) :: rest => math.exp(a) :: rest }
  primS("floor") { case FloatVal(a) :: rest => math.floor(a) :: rest }
  primTodo("frexp")
  primS("ldexp") { case IntVal(b) :: FloatVal(a) :: rest => (a * math.pow(2, b)) :: rest }
  primS("log") { case FloatVal(a) :: rest => math.log(a) :: rest }
  primS("log10") { case FloatVal(a) :: rest => math.log10(a) :: rest }
  primTodo("modf")
  primS("pow") { case FloatVal(b) :: FloatVal(a) :: rest => math.pow(a, b) :: rest }
  primS("sin") { case FloatVal(a) :: rest => math.sin(a) :: rest }
  primS("sinh") { case FloatVal(a) :: rest => math.sinh(a) :: rest }
  primS("sqrt") { case FloatVal(a) :: rest => math.sqrt(a) :: rest }
  primS("tan") { case FloatVal(a) :: rest => math.tan(a) :: rest }
  primS("tanh") { case FloatVal(a) :: rest => math.tanh(a) :: rest }
  primTodo("trunc")
  primTodo("localtime")
  primTodo("gmtime")
  primTodo("mktime")
  primTodo("strftime")
  primS("strtol") { case IntVal(rdx) :: StrVal(str) :: rest => Integer.parseInt(str, rdx.toInt) :: rest }
  primS("strtod") { case StrVal(str) :: rest => str.toDouble :: rest }
  primTodo("format")
  primTodo("formatf")
  primTodo("srand")
  primTodo("pred")
  primTodo("succ")
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
  primTodo("cons")
  primTodo("swons")
  primTodo("first")
  primTodo("rest")
  primTodo("compare")
  primTodo("at")
  primTodo("of")
  primTodo("size")
  primTodo("opcase")
  primTodo("case")
  primTodo("uncons")
  primTodo("unswons")
  primTodo("drop")
  primTodo("take")
  primTodo("concat")
  primTodo("enconcat")
  primTodo("name")
  primTodo("intern")
  primTodo("body")
  primTodo("null")
  primTodo("small")
  primTodo(">=")
  primTodo(">")
  primTodo("<=")
  primTodo("<")
  primTodo("!=")
  primTodo("=")
  primTodo("equal")
  primTodo("has")
  primTodo("in")
  primTodo("integer")
  primTodo("char")
  primTodo("logical")
  primTodo("set")
  primTodo("string")
  primTodo("list")
  primTodo("leaf")
  primTodo("user")
  primTodo("float")
  primTodo("file")
  primTodo("i")
  primTodo("x")
  primTodo("dip")
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
  primTodo("branch")
  primTodo("ifte")
  primTodo("ifinteger")
  primTodo("ifchar")
  primTodo("iflogical")
  primTodo("ifset")
  primTodo("ifstring")
  primTodo("iflist")
  primTodo("iffloat")
  primTodo("iffile")
  primTodo("cond")
  primTodo("while")
  primTodo("linrec")
  primTodo("tailrec")
  primTodo("binrec")
  primTodo("genrec")
  primTodo("condnestrec")
  primTodo("condlinrec")
  primTodo("step")
  primTodo("fold")
  primTodo("map")
  primTodo("times")
  primTodo("infra")
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
  primTodo("setautoput")
  primTodo("setundeferror")
  primTodo("setecho")
  primTodo("gc")
  primTodo("system")
  primTodo("getenv")
  primTodo("argv")
  primTodo("argc")
  primTodo("get")
  primTodo("put")
  primTodo("putch")
  primEffS("putchars") { case StrVal(str) :: _ => print(str) }
  primTodo("include")
  primTodo("abort")
  primTodo("quit")
}
