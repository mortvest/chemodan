package chemodan.interpreter

import scala.collection.mutable._
import scala.io.StdIn.readInt
import chemodan.lexer.{ChemodanLexer, ChemodanToken}
import chemodan.parser._
import scala.util.parsing.input.Positional

final case class Error(private val message: String = "",
  private val cause: Throwable = None.orNull) extends Exception(message, cause)

object ChemodanInterpreter {
  def raise(tree: Positional, msg: String){
    val errorMsg = "\n(" + tree.pos + ") " + msg
    throw new Error(errorMsg)
  }
  // def apply(code: String): Either[ChemodanInterpretationError, ChemodanAST] = {
  def apply(code: String, rev: Boolean): Unit = {
    val toks =
      for {
        tokens <- ChemodanLexer(code).right
      } yield tokens
    // println(toks)
    val tree =
      for {
        tokens <- ChemodanLexer(code).right
        ast <- ChemodanParser(tokens).right
      } yield ast
    // println(tree)
    tree match {
      case Right(tr) => if (rev) interpret(reverse(tr)) else interpret(tr)
      case Left(msg) => println(msg)
    }
  }
  // Reverse a given AST
  def reverse(tree: ChemodanAST): ChemodanAST = {
    tree match {
      case AndThen(step1, step2) => AndThen(reverse(step2), reverse(step1))
      case Procedure(name, stat) if name=="main" => Procedure(name, reverse(stat))
      case Procedure(name, stat) => Procedure(name, stat)
      case Print(varName) => Read(varName)
      case Read(varName) => Print(varName)
      case Call(procName) => Uncall(procName)
      case Uncall(procName) => Call(procName)
      case AssignMinus(varName, exp) => AssignPlus(varName, exp)
      case AssignPlus(varName, exp) => AssignMinus(varName, exp)
      // ???
      case IfThen(predicate, thenBlock, elseBlock, fipredicate) =>
        IfThen(fipredicate, reverse(thenBlock), reverse(elseBlock), predicate)
      case Repeat(fromExp, block, untilExp) => Repeat(untilExp, reverse(block), fromExp)
      case LocalVar(nameAlloc, expAlloc, block, nameFree, expFree) =>
        LocalVar(nameFree, expFree, reverse(block), nameAlloc, expAlloc)
      case n => n
    }
  }
  // Execute a statement (in a kinda hackish way, so no monads are required :P)
  def exec(tree: ChemodanAST, varMap: Map[String, Int], procMap: Map[String, ChemodanAST]): Unit = {
    tree match {
      case Call(procName) =>
        if (procName == "main") raise(tree,"Main procedure is not callable from program")
        (procMap get procName) match {
          case Some(proc) => exec(proc, varMap, procMap)
          case None => raise(tree,s"Procedure $procName can't be found")
      }
      case Uncall(procName) =>
        if (procName == "main") raise(tree,"Main procedure is not callable from program")
        (procMap get procName) match {
        case Some(proc) => exec(reverse(proc), varMap, procMap)
        case None => raise(tree,s"Procedure $procName can not be found")
      }
      case Print(varName) =>
        val value = eval(Variable(varName), varMap)
        println(s"$varName = $value")
        varMap.update(varName, 0)
      case Read(varName) =>
        eval(Variable(varName), varMap) match {
          case 0 =>
            try {
              print(s"$varName = ")
              val input = readInt()
              varMap.update(varName, input)
            } catch {
              case e: Exception => raise(tree,s"The value provided is not an integer")
            }
          case _ => raise(tree,s"Can not read into $varName (is nonzero)")
        }
      case AndThen(step1, step2) => exec(step1, varMap, procMap); exec(step2, varMap, procMap)
      case LocalVar(nameAlloc, expAlloc, block, nameFree, expFree) =>
        if (nameAlloc != nameFree)
          raise(tree,s"Alloc name: $nameAlloc does not match free: $nameFree")
        if (treeSearch(expAlloc, nameAlloc))
          raise(tree,s"Variable $nameAlloc is used in its own allocation expression")
        alloc(nameAlloc, eval(expAlloc, varMap), varMap)
        // println(varMap)
        exec(block, varMap, procMap)
        if (treeSearch(expFree, nameAlloc))
          raise(tree,s"Variable $nameAlloc is used in its own deallocation expression")
        if (eval(Variable(nameAlloc),varMap) != eval(expFree, varMap))
          raise(tree, s"Deallocation expression and the value of $nameAlloc do not match")
        free(nameAlloc, varMap)
        // println(varMap)
      case AssignPlus(varName, exp)  => varUpdate(varName, exp, varMap, _+_)
      case AssignMinus(varName, exp) => varUpdate(varName, exp, varMap, _-_)
      case Swap(varName1, varName2) =>
        // if (varName1 == varName2) throw new Error(s"Swapping variable $varName1 with itself")
        val fst = eval(Variable(varName1), varMap)
        val snd = eval(Variable(varName2), varMap)
        varMap.update(varName1, snd)
        varMap.update(varName2, fst)
      case Repeat(fromExp, block, untilExp) =>
        if (eval(fromExp, varMap) == 0) raise(tree,"Loop enter condition is 0")
        do {
          exec(block, varMap, procMap)
        }
        while (eval(untilExp, varMap) == 0)
      case IfThen(ifcond, thenBlock, elseBlock, ficond) =>
        val ifCond = eval(ifcond, varMap) != 0
        if (ifCond) {
          exec(thenBlock, varMap, procMap)
        }else{
          exec(elseBlock, varMap, procMap)
        }
        if ((eval(ficond, varMap) != 0) != ifCond) {
          raise(tree,"FI predicate does not match IF")
        }
      case _ => throw new Error(s"Unknown error while executing")
    }
  }
  // Find the prefix of the latest hidden variable with the same name
  def findConst(varName: String, varMap: Map[String, Int]) = {
    def find(n: Int): Int = {
      val name = n.toString + varName
        (varMap get name) match {
        case Some(_) => find(n+1)
        case None => n
      }
    }
    find(0)
  }
  // Allocate a local variable
  def alloc(varName: String, newVal: Int, varMap: Map[String, Int]) = {
    (varMap get varName) match {
      // There is a variable with the same name
      case Some(oldValue) =>
        varMap.update(findConst(varName, varMap).toString + varName, oldValue) //Hide the old value
        varMap.update(varName, newVal) //Save the new value
      // There are no variables with the same name
      case None => varMap.update(varName, newVal)
    }
  }

  // Deallocate a local variable
  def free(varName: String, varMap: Map[String, Int]) = {
    (varMap get "0"+varName) match {
      // A variable with the same name was hidden before - we need to restore it
      case Some(_) =>
        val hiddenKey = (findConst(varName, varMap) - 1).toString + varName
        val hiddenVal = varMap.apply(hiddenKey)
        varMap.remove(hiddenKey)
        varMap.update(varName, hiddenVal)
      //There are no variables hidden with the same name - delete
      case None => varMap.remove(varName)
    }
  }

  // Add all procedures/global variables to the hash tables
  def collect(tree: ChemodanAST, varMap: Map[String, Int], procMap: Map[String, ChemodanAST]): Unit = {
    tree match {
      case AndThen(step1, step2) => collect(step1, varMap, procMap); collect(step2, varMap, procMap)
      case Procedure(name, stat) => procMap.update(name, stat)
      case GlobalVar(varName) => (varMap get varName) match {
        case Some(_) => raise(tree,s"Variable $varName has already been defined")
        case None => varMap.update(varName, 0)
      }
      case _ => raise(tree,s"Statement outside procedure")
    }
  }

  // Pure function, evaluates expressions
  def eval(tree: Expression, map: Map[String, Int]): Int = {
    tree match {
      case Number(value) => value
      case Variable(varName) => (map get varName) match {
        case Some(x) => x
        case None => throw new Error(s"Variable $varName is not defined")
      }
      case Equals(exp1, exp2) => if(eval(exp1, map) == eval(exp2, map)) 1 else 0
      case Less(exp1, exp2)   => if(eval(exp1, map) < eval(exp2, map)) 1 else 0
      case Plus(exp1, exp2)   => eval(exp1, map) + eval(exp2, map)
      case Minus(exp1, exp2)  => eval(exp1, map) - eval(exp2, map)
      case Mult(exp1, exp2)   => eval(exp1, map) * eval(exp2, map)
      case Div(exp1, exp2)    => eval(exp1, map) / eval(exp2, map)
      case Mod(exp1, exp2)    => eval(exp1, map) % eval(exp2, map)
    }
  }

  def zeroCheck(map: Map[String, Int]) = {
    // Create an error message
    def compMsg(keys: List[String], values:List[Int]): String = {
      (keys, values) match {
        case (k::Nil, v::Nil) => s"$k = $v"
        case (k::ks, v::vs) => s"$k = $v, " + compMsg(ks, vs)
        case _ => ""
      }
    }
    val nonZero = map.filter({case (_, num) => num != 0})
    val str = compMsg(nonZero.keys.toList, nonZero.values.toList)
    if (nonZero.size > 0) {
      throw new Error(s"Nonzero variables at program end: {$str}")
    }
  }

  // Try finding a variable in a given expression
  def treeSearch(tree: Expression, varName: String): Boolean = {
    def search(exp1: Expression, exp2: Expression): Boolean = {
      treeSearch(exp1, varName) || treeSearch(exp2, varName)
    }
    // Ugly, but still the best way in Scala
    tree match {
      case Variable(name)     => name == varName
      case Equals(exp1, exp2) => search(exp1, exp2)
      case Less(exp1, exp2)   => search(exp1, exp2)
      case Plus(exp1, exp2)   => search(exp1, exp2)
      case Minus(exp1, exp2)  => search(exp1, exp2)
      case Mult(exp1, exp2)   => search(exp1, exp2)
      case Div(exp1, exp2)    => search(exp1, exp2)
      case Mod(exp1, exp2)    => search(exp1, exp2)
      case _ => false
    }
  }

  def varUpdate(varName: String, exp: Expression, varMap: Map[String, Int], func:(Int,Int)=>Int) = {
    treeSearch(exp, varName) match {
      case false =>
        val prev = eval(Variable(varName), varMap)
        val res = func(prev, eval(exp, varMap))
        varMap.update(varName, res)
      case _ =>
        throw new Error(s"Value of $varName is used in its own update")
    }
  }

  def interpret(tree: ChemodanAST) = {
    // val varMap = HashMap("w" -> 12, "j" -> 34, "k" -> 51)
    val varMap = HashMap.empty[String, Int] // Mutable variable map
    val procMap = HashMap.empty[String, ChemodanAST] // Mutable procedure map
    collect(tree, varMap, procMap)
    // println(procMap)
      (procMap get "main") match {
      case Some(main) => exec(main, varMap, procMap)
      case None => throw new Error("Main procedure can not be found")
    }
    zeroCheck(varMap)
  }
}
