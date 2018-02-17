package net.ruippeixotog.joyful.lang

import java.io.File

import scala.io.Source
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

import Ast._

// http://www.kevinalbrecht.com/code/joy-mirror/j09imp.html, "Factors and terms"
object Parser extends StandardTokenParsers {
  override val lexical = Lexical

  def parseFile(file: File): Program =
    parseFile(file.getAbsolutePath)

  def parseFile(filename: String): Program =
    parse(Source.fromFile(filename).mkString)

  def parseResource(name: String): Program =
    parse(Source.fromResource(name).mkString)

  def parse(source: String): Program = {
    phrase(program)(new lexical.Scanner(source)) match {
      case Success(prog, _) => prog
      case noSuccess => throw new Exception(s"$source: $noSuccess")
    }
  }

  def program: Parser[Program] =
    rep(compoundDef ^^ ModuleDef | term <~ ("END" | ".") ^^ EvalExpr) ^^ Program

  def compoundDef: Parser[CompoundDef] =
    opt("MODULE" ~> ident) ~
      opt(("PRIVATE" | "HIDE") ~> defSeq) ~
      opt(("PUBLIC" | "IN" | "LIBRA" | "DEFINE") ~> defSeq) <~
      ("END" | ".") ^^ { case nameOpt ~ pvt ~ pub => CompoundDef(nameOpt, pvt.getOrElse(Nil), pub.getOrElse(Nil)) }

  def simpleDef: Parser[SimpleDef] =
    ident ~ "==" ~ term ^^ { case n ~ _ ~ v => SimpleDef(n, v) }

  def defSeq: Parser[List[Definition]] =
    rep1sep(simpleDef | compoundDef, ";")

  def term: Parser[Term] =
    rep(factor) ^^ Term

  def factor: Parser[Factor] = {
    def numericOrCharLit: Parser[Int] =
      numericLit ^^ (_.toInt) | charLit ^^ (_.toInt)

    ident ^^ FIdent |
      numericLit ^^ { x => FNumber(x.toDouble) } |
      charLit ^^ FChar |
      stringLit ^^ FStr |
      "{" ~> rep(numericOrCharLit) <~ "}" ^^ { seq => FSet(seq.toSet) } |
      "[" ~> term <~ "]" ^^ FQuoted
  }

  def charLit: Parser[Char] =
    elem("char", _.isInstanceOf[Lexical.CharLit]) ^^ (_.chars.head)
}
