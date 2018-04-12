package chemodan.lexer

import chemodan.interpreter.{Location, ChemodanLexerError}
import scala.util.parsing.combinator.RegexParsers

object ChemodanLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def apply(code: String): Either[ChemodanLexerError, List[ChemodanToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(ChemodanLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def tokens: Parser[List[ChemodanToken]] = {
    phrase(rep1(procedure | global | ifStatement | thenStatement | elseStatement
      | fiStatement | procCall | procUncall | start | loop | doStmt | until | alloc | free | swap
      | printVal | readVal | procUncall | assignPlus | assignMinus | number | neq | leq | less | equals
      | plus | sqbLeft | sqbRight
      | minus | mult | div | mod | parLeft | parRight | identifier | indentation)) ^^ { rawTokens =>
      processIndentations(rawTokens)
    }
  }

  private def processIndentations(tokens: List[ChemodanToken],
                                  indents: List[Int] = List(0)): List[ChemodanToken] = {
    tokens.headOption match {

      // if there is an increase in indentation level, we push this new level into the stack
      // and produce an INDENT
      case Some(INDENTATION(spaces)) if spaces > indents.head =>
        INDENT() :: processIndentations(tokens.tail, spaces :: indents)

      // if there is a decrease, we pop from the stack until we have matched the new level and
      // we produce a DEDENT for each pop
      case Some(INDENTATION(spaces)) if spaces < indents.head =>
        val (dropped, kept) = indents.partition(_ > spaces)
        (dropped map (_ => DEDENT())) ::: processIndentations(tokens.tail, kept)

      // if the indentation level stays unchanged, no tokens are produced
      case Some(INDENTATION(spaces)) if spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      // other tokens are ignored
      case Some(token) =>
        token :: processIndentations(tokens.tail, indents)

      // the final step is to produce a DEDENT for each indentation level still remaining, thus
      // "closing" the remaining open INDENTS
      case None =>
        indents.filter(_ > 0).map(_ => DEDENT())

    }
  }

  def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }


	def number: Parser[NUMBER] = positioned {
		("[-]?[1-9][0-9]*".r | "[-]?[0-9]+".r) ^^ { s => NUMBER(s.toInt) }
  }

  def indentation: Parser[INDENTATION] = positioned {
    "\n[ ]*".r ^^ { whitespace =>
      val nSpaces = whitespace.length - 1
      INDENTATION(nSpaces)
    }
  }

  def global        = positioned { "global "       ^^ (_ => GLOBAL()) }
  def procedure     = positioned { "procedure "    ^^ (_ => PROCEDURE()) }
  def printVal      = positioned { "print "        ^^ (_ => PRINT()) }
  def readVal       = positioned { "read "         ^^ (_ => READ()) }
  def ifStatement   = positioned { "if "           ^^ (_ => IF()) }
  def thenStatement = positioned { "then"          ^^ (_ => THEN()) }
  def elseStatement = positioned { "else"          ^^ (_ => ELSE()) }
  def fiStatement   = positioned { "fi "           ^^ (_ => FI()) }
  def start         = positioned { "start "        ^^ (_ => START()) }
  def loop          = positioned { "loop"          ^^ (_ => LOOP()) }
  def doStmt        = positioned { "do"            ^^ (_ => DOSTMT()) }
  def until         = positioned { "until "        ^^ (_ => UNTIL()) }
  def assignPlus    = positioned { "+="            ^^ (_ => ASSIGNPLUS()) }
  def assignMinus   = positioned { "-="            ^^ (_ => ASSIGNMINUS()) }
  def procCall      = positioned { "call"          ^^ (_ => CALL()) }
  def procUncall    = positioned { "uncall "       ^^ (_ => UNCALL()) }
  def alloc         = positioned { "alloc "        ^^ (_ => ALLOC()) }
  def free          = positioned { "free "         ^^ (_ => FREE()) }
  def swap          = positioned { "<=>"           ^^ (_ => SWAP()) }
  def equals        = positioned { "="             ^^ (_ => EQUALS()) }
  def less          = positioned { "<"             ^^ (_ => LESS()) }
  def leq           = positioned { "<="            ^^ (_ => LEQ()) }
  def neq           = positioned { "!="            ^^ (_ => NEQ()) }
  def plus          = positioned { "+"             ^^ (_ => PLUS()) }
  def minus         = positioned { "-"             ^^ (_ => MINUS()) }
  def mult          = positioned { "*"             ^^ (_ => MULT()) }
  def div           = positioned { "/"             ^^ (_ => DIV()) }
  def mod           = positioned { "%"             ^^ (_ => MOD()) }
  def parLeft       = positioned { "("             ^^ (_ => PLEFT()) }
  def parRight      = positioned { ")"             ^^ (_ => PRIGHT()) }
  def sqbLeft       = positioned { "["             ^^ (_ => SQBLEFT()) }
  def sqbRight      = positioned { "]"             ^^ (_ => SQBRIGHT()) }
}
