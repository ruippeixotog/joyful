package net.ruippeixotog.joyful.lang

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh

// http://www.kevinalbrecht.com/code/joy-mirror/j09imp.html, "Tokens"
object Lexical extends StdLexical {

  case class CharLit(chars: String) extends Token {
    override def toString = chars
  }

  override def token: Parser[Token] =
    numericLit | charLit | stringLit | identifier | EofCh ^^^ EOF |
    '\'' ~> failure("unclosed string literal") |
    '\"' ~> failure("unclosed string literal") |
    delim |
    failure("illegal character")

  def identifier: Parser[Token] =
    rep1(identFirstChar, identRestChar) ^^ { chars => processIdent(chars.mkString) }

  def numericLit: Parser[Token] =
    opt('-') ~ rep1(digit) ~ opt('.' ~> rep1(digit)) ^^ {
      case sign ~ intPart ~ None => NumericLit(sign.mkString + intPart.mkString)
      case sign ~ intPart ~ Some(fracPart) => NumericLit(sign.mkString + (intPart ::: '.' :: fracPart).mkString)
    }

  def charLit: Parser[Token] =
    '\'' ~> char('\'') ^^ { char => CharLit(char.toString) }

  def stringLit: Parser[Token] =
    '\"' ~> rep(char('\"')) <~ '\"' ^^ { chars => StringLit(chars.mkString) }

  // -----

  def identFirstChar: Parser[Char] =
    chrExcept('[', ']', '{', '}', ';', '.', '\'', '"').filter(!_.isDigit)

  def identRestChar: Parser[Char] =
    letter | digit | '=' | '_' | '-'

  def char(closeChar: Char): Parser[Char] =
    escapedChar | chrExcept(closeChar, '\n', EofCh)

  def escapedChar: Parser[Char] =
    '\\' ~> (
      'n' ^^^ '\n' | 't' ^^^ '\t' | 'b'  ^^^ '\b' | 'r' ^^^ '\r' | 'f'  ^^^ '\f' | '\'' | '\"' |
        repN(3, digit) ^^ { ds => ds.mkString.toInt.toChar })

  override def whitespace: Parser[Any] = rep[Any](
    whitespaceChar |
      '(' ~ '*' ~ comment |
      '#' ~ rep(chrExcept(EofCh, '\n')) |
      '(' ~ '*' ~ failure("unclosed comment"))

  override def comment: Parser[Any] =
    rep(chrExcept(EofCh, '*')) ~ '*' ~ ')' ^^^ ' ' |
      rep(chrExcept(EofCh, '*')) ~ '*' ~ comment ^^^ ' '

  delimiters += ("[", "]", "{", "}", ";", ".")
  reserved += ("==", "MODULE", "PRIVATE", "PUBLIC", "END", "HIDE", "IN", "DEFINE", "LIBRA")
}
