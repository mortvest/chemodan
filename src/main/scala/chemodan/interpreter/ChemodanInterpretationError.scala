package chemodan.interpreter

sealed trait ChemodanInterpretationError

case class ChemodanLexerError(location: Location, msg: String) extends ChemodanInterpretationError
case class ChemodanParserError(location: Location, msg: String) extends ChemodanInterpretationError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}
