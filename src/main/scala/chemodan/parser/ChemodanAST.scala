package chemodan.parser

import scala.util.parsing.input.Positional

sealed trait ChemodanAST extends Positional
case class GlobalVar(varName: String) extends ChemodanAST
case class Procedure(name: String, stat: ChemodanAST) extends ChemodanAST
case class Print(valName: String) extends ChemodanAST
case class Read(valName: String) extends ChemodanAST
case class AndThen(step1: ChemodanAST, step2: ChemodanAST) extends ChemodanAST
case class IfThen(predicate: Expression, thenBlock: ChemodanAST,
  elseBlock: ChemodanAST, fipredicate: Expression ) extends ChemodanAST
case class Repeat(from: Expression, block: ChemodanAST, until: Expression) extends ChemodanAST
case class RevRepeat(block: ChemodanAST, whilePredicate: Expression) extends ChemodanAST
case class AssignPlus(valName: String, value: Expression) extends ChemodanAST
case class AssignMinus(valName: String, value: Expression) extends ChemodanAST
case class Call(procName: String) extends ChemodanAST
case class Uncall(procName: String) extends ChemodanAST
case class LocalVar(varName: String, expDef: Expression, block: ChemodanAST,
  freeName: String, expFree: Expression) extends ChemodanAST
case class Swap(varName1: String, varName2: String) extends ChemodanAST

sealed trait Expression extends Positional
case class Number(value: Int) extends Expression
case class Variable(varName: String) extends Expression
case class Equals(exp1: Expression, exp2: Expression) extends Expression
case class Less(exp1: Expression, exp2: Expression) extends Expression
case class Leq(exp1: Expression, exp2: Expression) extends Expression
case class Neq(exp1: Expression, exp2: Expression) extends Expression
case class Plus(exp1: Expression, exp2: Expression) extends Expression
case class Minus(exp1: Expression, exp2: Expression) extends Expression
case class Mult(exp1: Expression, exp2: Expression) extends Expression
case class Div(exp1: Expression, exp2: Expression) extends Expression
case class Mod(exp1: Expression, exp2: Expression) extends Expression
