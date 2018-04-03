package chemodan.lexer

import scala.util.parsing.input.Positional

sealed trait ChemodanToken extends Positional

case class IDENTIFIER(str: String) extends ChemodanToken
case class NUMBER(value: Int) extends ChemodanToken
case class INDENTATION(spaces: Int) extends ChemodanToken
case class INDENT() extends ChemodanToken
case class DEDENT() extends ChemodanToken

case class GLOBAL() extends ChemodanToken
case class PROCEDURE() extends ChemodanToken
case class PRINT() extends ChemodanToken
case class READ() extends ChemodanToken
case class IF() extends ChemodanToken
case class THEN() extends ChemodanToken
case class ELSE() extends ChemodanToken
case class FI() extends ChemodanToken
case class START() extends ChemodanToken
case class LOOP() extends ChemodanToken
case class UNTIL() extends ChemodanToken
case class ASSIGNPLUS() extends ChemodanToken
case class ASSIGNMINUS() extends ChemodanToken
case class CALL() extends ChemodanToken
case class UNCALL() extends ChemodanToken
case class ALLOC() extends ChemodanToken
case class FREE() extends ChemodanToken
case class SWAP() extends ChemodanToken

case class EQUALS() extends ChemodanToken
case class LESS() extends ChemodanToken
case class PLUS() extends ChemodanToken
case class MINUS() extends ChemodanToken
case class MULT() extends ChemodanToken
case class DIV() extends ChemodanToken
case class MOD() extends ChemodanToken
case class PLEFT() extends ChemodanToken
case class PRIGHT() extends ChemodanToken
