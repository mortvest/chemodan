package chemodan.parser

import chemodan.interpreter.{Location, ChemodanParserError}
import chemodan.lexer._
import scala.util.parsing.combinator.{Parsers, PackratParsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

object ChemodanParser extends Parsers with PackratParsers{
  override type Elem = ChemodanToken

  class ChemodanTokenReader(tokens: Seq[ChemodanToken]) extends Reader[ChemodanToken] {
    override def first: ChemodanToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[ChemodanToken] = new ChemodanTokenReader(tokens.tail)
  }

  def apply(tokens: Seq[ChemodanToken]): Either[ChemodanParserError, ChemodanAST] = {
    val reader = new ChemodanTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(ChemodanParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def structure: Parser[ChemodanAST] = positioned {
    val procedure = PROCEDURE() ~ identifier ~ INDENT() ~ block ~ DEDENT() ^^ {
      case _ ~ IDENTIFIER(name) ~ _ ~ stmt ~ _ => Procedure(name, stmt)
    }
    val global = GLOBAL() ~ identifier ^^ { case _ ~ IDENTIFIER(name) => GlobalVar(name) }
    val globalArr = GLOBAL() ~ identifier ~ SQBLEFT() ~ number ~ SQBRIGHT() ^^ {
      case _ ~ IDENTIFIER(name) ~ _ ~ NUMBER(num) ~ _ => GlobalArray(name, num) }
    procedure | globalArr | global
  }

  def program: Parser[ChemodanAST] = positioned {
    rep1(structure) ^^ { case strucList => strucList reduceRight AndThen }
  }

  def block: Parser[ChemodanAST] = positioned {
    rep1(statement) ^^ { case stmtList => stmtList reduceRight AndThen }
  }

  def statement: Parser[ChemodanAST] = positioned {
    val procCall   = CALL()   ~ identifier ^^ { case _ ~ IDENTIFIER(procName) => Call(procName) }
    val procUncall = UNCALL() ~ identifier ^^ { case _ ~ IDENTIFIER(procName) => Uncall(procName) }
    val ifThen = IF() ~ expression ~ THEN() ~ INDENT() ~ block ~ DEDENT() ~
    ELSE() ~ INDENT() ~ block ~ DEDENT() ~ FI() ~ expression  ^^ {
      case _ ~ ifcond ~ _ ~ _ ~ thenblock ~ _ ~ _ ~ _ ~ elseblock ~ _ ~ _ ~ ficond =>
        IfThen(ifcond, thenblock, elseblock, ficond)
    }
    val repeatLoop = START() ~ expression ~ DOSTMT() ~ INDENT() ~ block ~ DEDENT() ~
    UNTIL() ~ expression ^^ {
      case _ ~ fromExp ~ _ ~ _ ~ loopStat ~ _ ~ _ ~ untilExp => Repeat(fromExp, loopStat, untilExp)
    }
    val altLoop = START() ~ expression ~ LOOP() ~ INDENT() ~ block ~ DEDENT() ~
    UNTIL() ~ expression ^^ {
      case _ ~ fromExp ~ _ ~ _ ~ loopStat ~ _ ~ _ ~ untilExp => Loop(fromExp, loopStat, untilExp)
    }

    val assignPlus = identifier ~ ASSIGNPLUS() ~ expression ^^ {
      case IDENTIFIER(id) ~ _ ~ exp => AssignPlus(id, exp)
    }
    val assignMinus = identifier ~ ASSIGNMINUS() ~ expression ^^ {
      case IDENTIFIER(id) ~ _ ~ exp => AssignMinus(id, exp)
    }
    val assignMinusArr = identifier ~ SQBLEFT() ~ expression ~ SQBRIGHT() ~ ASSIGNMINUS() ~ expression ^^ {
      case IDENTIFIER(id) ~ _ ~ exp1 ~ _ ~ _ ~ exp2 => AssignMinusArr(id, exp1, exp2)
    }
    val assignPlusArr = identifier ~ SQBLEFT() ~ expression ~ SQBRIGHT() ~ ASSIGNPLUS() ~ expression ^^ {
      case IDENTIFIER(id) ~ _ ~ exp1 ~ _ ~ _ ~ exp2 => AssignPlusArr(id, exp1, exp2)
    }
    val printVal = PRINT() ~ identifier ^^ { case _ ~ IDENTIFIER(id) => Print(id) }
    val readVal  = READ()  ~ identifier ^^ { case _ ~ IDENTIFIER(id) => Read(id) }
    val printArr = PRINT() ~ identifier ~ SQBLEFT() ~ expression ~ SQBRIGHT() ^^ {
      case _ ~ IDENTIFIER(id) ~ _ ~ exp ~ _ => PrintArr(id, exp)
    }
    val readArr = READ() ~ identifier ~ SQBLEFT() ~ expression ~ SQBRIGHT() ^^ {
      case _ ~ IDENTIFIER(id) ~ _ ~ exp ~ _ => ReadArr(id, exp)
    }
    val swap = identifier ~ SWAP() ~ identifier ^^ {
      case  IDENTIFIER(id1) ~ _ ~ IDENTIFIER(id2) => Swap(id1, id2)
    }
    val swapAV = identifier ~ SQBLEFT() ~ expression ~ SQBRIGHT() ~ SWAP() ~ identifier ^^ {
      case IDENTIFIER(idArr) ~ _ ~ exp ~ _ ~ _ ~ IDENTIFIER(idVal) => SwapAV(idArr, exp, idVal)
    }
    val swapVA = identifier ~ SWAP() ~ identifier ~ SQBLEFT() ~ expression ~ SQBRIGHT() ^^ {
      case IDENTIFIER(idVal) ~ _ ~ IDENTIFIER(idArr) ~ _ ~ exp ~ _  => SwapAV(idArr, exp, idVal)
    }
    val swapAA = identifier ~ SQBLEFT() ~ expression ~ SQBRIGHT() ~ SWAP() ~ identifier ~
    SQBLEFT() ~ expression ~ SQBRIGHT() ^^ {
      case IDENTIFIER(idArr1) ~ _ ~ exp1 ~ _ ~ _ ~ IDENTIFIER(idArr2) ~ _ ~ exp2 ~ _  =>
        SwapAA(idArr1, exp1, idArr2, exp2)
    }
    val localVar = ALLOC() ~ identifier ~ EQUALS() ~ expression ~
    INDENT() ~ block ~ DEDENT() ~ FREE() ~ identifier ~ EQUALS() ~ expression ^^ {
      case _ ~ IDENTIFIER(id1) ~ _ ~ exp1 ~ _ ~ blk ~ _ ~ _ ~ IDENTIFIER(id2) ~ _ ~ exp2 =>
        LocalVar(id1, exp1, blk, id2, exp2)
    }
    procCall | procUncall | ifThen | altLoop | repeatLoop | assignPlusArr | assignMinusArr | swapAA | swapVA | swapAV | assignPlus | assignMinus | printArr | readArr | printVal | readVal | swap | localVar
  }

  def value: Parser[Expression] = positioned {
    val arrElement = identifier ~ SQBLEFT() ~ expression ~ SQBRIGHT() ^^ {
      case IDENTIFIER(id) ~ _ ~ exp ~ _ => ArrElement(exp, id) }
    val literal = number ^^ { case NUMBER(num) => Number(num) }
    val variable = identifier ^^ { case IDENTIFIER(id) => Variable(id) }
    arrElement | literal | variable | PLEFT() ~> expression <~ PRIGHT()
  }

  lazy val expression: Parser[Expression] = formula ~ rep((EQUALS() | LESS() | LEQ() | NEQ()) ~ formula) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, LESS() ~ t2) => Less(t1, t2)
      case (t1, EQUALS() ~ t2) => Equals(t1, t2)
      case (t1, NEQ() ~ t2) => Neq(t1, t2)
      case (t1, LEQ() ~ t2) => Leq(t1, t2)
    }
  }

  lazy val formula = term ~ rep((PLUS() | MINUS()) ~ term) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, PLUS() ~ t2) => Plus(t1, t2)
      case (t1, MINUS() ~ t2) => Minus(t1, t2)
    }
  }

  lazy val term = value ~ rep((MULT() | DIV() | MOD()) ~ value) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, MULT() ~ t2) => Mult(t1, t2)
      case (t1, DIV() ~ t2) => Div(t1, t2)
      case (t1, MOD() ~ t2) => Mod(t1, t2)
    }
  }

  private def identifier: Parser[IDENTIFIER] = positioned {
    accept("identifier", { case id @ IDENTIFIER(name) => id })
  }

  private def number: Parser[NUMBER] = positioned {
    accept("number", { case num @ NUMBER(name) => num })
  }
}


// package chemodan.parser

// import chemodan.interpreter.{Location, ChemodanParserError}
// import chemodan.lexer._
// import scala.util.parsing.combinator.{Parsers, PackratParsers, RegexParsers}
// import scala.util.parsing.input.{NoPosition, Position, Reader}

// object ChemodanParser extends Parsers with PackratParsers{
// }
