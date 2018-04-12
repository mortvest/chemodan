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
  def apply(code: String, rev: Boolean, debug: Boolean): Unit = {
    val toks =
      for {
        tokens <- ChemodanLexer(code).right
      } yield tokens
    val tree =
      for {
        tokens <- ChemodanLexer(code).right
        ast <- ChemodanParser(tokens).right
      } yield ast
    if (debug) {
      println("TOKENS:")
      println(toks)
      println("AST:")
      println(tree)
      println("PROGRAM:")
    }
    tree match {
      case Right(tr) => if (rev) interpret(reverse(tr)) else interpret(tr)
      // case Left(msg) => println(msg)
      case Left(msg) => throw new Error(msg.toString)
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
      case PrintArr(arrName, index) => ReadArr(arrName, index)
      case ReadArr(arrName, index) => PrintArr(arrName, index)
      case Call(procName) => Uncall(procName)
      case Uncall(procName) => Call(procName)
      case AssignMinus(varName, exp) => AssignPlus(varName, exp)
      case AssignPlus(varName, exp) => AssignMinus(varName, exp)
      case AssignMinusArr(arrName, index, exp) => AssignPlusArr(arrName, index, exp)
      case AssignPlusArr(arrName, index, exp) => AssignMinusArr(arrName, index, exp)
      case IfThen(predicate, thenBlock, elseBlock, fipredicate) =>
        IfThen(fipredicate, reverse(thenBlock), reverse(elseBlock), predicate)
      case Repeat(fromExp, block, untilExp) => Repeat(untilExp, reverse(block), fromExp)
      case Loop(fromExp, block, untilExp) => Loop(untilExp, reverse(block), fromExp)
      case LocalVar(nameAlloc, expAlloc, block, nameFree, expFree) =>
        LocalVar(nameFree, expFree, reverse(block), nameAlloc, expAlloc)
      case n => n
    }
  }
  // Execute a statement (in a kinda hackish way, so no monads are required :P)
  def exec(tree: ChemodanAST, varMap: Map[String, Int],
    procMap: Map[String, ChemodanAST], arrMap: Map[String, Array[Int]]): Unit = {
    tree match {
      case Call(procName) =>
        if (procName == "main") raise(tree,"Main procedure is not callable from program")
        (procMap get procName) match {
          case Some(proc) => exec(proc, varMap, procMap, arrMap)
          case None => raise(tree,s"Procedure $procName can't be found")
      }
      case Uncall(procName) =>
        if (procName == "main") raise(tree,"Main procedure is not callable from program")
        (procMap get procName) match {
        case Some(proc) => exec(reverse(proc), varMap, procMap, arrMap)
        case None => raise(tree,s"Procedure $procName can not be found")
      }
      case Print(varName) =>
        val value = eval(Variable(varName), varMap, arrMap)
        println(s"<< $varName = $value")
        varMap.update(varName, 0)
      case PrintArr(arrName, index) =>
        val ind = eval(index, varMap, arrMap)
        val value = eval(ArrElement(index, arrName), varMap, arrMap)
        val arr = arrMap.get(arrName).get
        println(s"<< $arrName[$ind] = $value")
        arr(ind) = 0
        arrMap.update(arrName, arr)
      case Read(varName) =>
        eval(Variable(varName), varMap, arrMap) match {
          case 0 =>
            try {
              print(s">> $varName = ")
              val input = readInt()
              varMap.update(varName, input)
            } catch {
              case e: Exception => raise(tree,s"The value provided is not an integer")
            }
          case _ => raise(tree,s"Can not read into $varName (is nonzero)")
        }
      case ReadArr(arrName, index) =>
        val ind = eval(index, varMap, arrMap)
        eval(ArrElement(index, arrName), varMap, arrMap) match {
          case 0 =>
            try {
              print(s">> $arrName[$ind] = ")
              val input = readInt()
              val arr = arrMap.get(arrName).get
              arr(ind) = input
              arrMap.update(arrName, arr)
            } catch {
              case e: Exception => raise(tree,s"The value provided is not an integer")
            }
          case _ => raise(tree,s"Can not read into $arrName[$ind] (is nonzero)")
        }
      case AndThen(step1, step2) =>
        exec(step1, varMap, procMap, arrMap);
        exec(step2, varMap, procMap, arrMap)
      case LocalVar(nameAlloc, expAlloc, block, nameFree, expFree) =>
        if (nameAlloc != nameFree)
          raise(tree,s"Alloc name: $nameAlloc does not match free: $nameFree")
        if (treeSearch(expAlloc, Variable(nameAlloc)))
          raise(tree,s"Variable $nameAlloc is used in its own allocation expression")
        alloc(nameAlloc, eval(expAlloc, varMap, arrMap), varMap)
        exec(block, varMap, procMap, arrMap)
        if (treeSearch(expFree, Variable(nameAlloc)))
          raise(tree,s"Variable $nameAlloc is used in its own deallocation expression")
        if (eval(Variable(nameAlloc),varMap, arrMap) != eval(expFree, varMap, arrMap))
          raise(tree, s"Deallocation expression and the value of $nameAlloc do not match")
        free(nameAlloc, varMap)
      case AssignPlus(varName, exp)  => varUpdate(varName, exp, varMap, _+_, arrMap)
      case AssignPlusArr(arrName, index, exp) => varUpdateArr(arrName, index, exp, varMap, _+_, arrMap)
      case AssignMinus(varName, exp) => varUpdate(varName, exp, varMap, _-_, arrMap)
      case AssignMinusArr(arrName, index, exp) => varUpdateArr(arrName, index, exp, varMap, _-_, arrMap)
      case Swap(varName1, varName2) =>
        // if (varName1 == varName2) throw new Error(s"Swapping variable $varName1 with itself")
        val fst = eval(Variable(varName1), varMap, arrMap)
        val snd = eval(Variable(varName2), varMap, arrMap)
        varMap.update(varName1, snd)
        varMap.update(varName2, fst)
      case SwapAV(arrName, index, varName) =>
        val fst = eval(ArrElement(index, arrName), varMap, arrMap)
        val snd = eval(Variable(varName), varMap, arrMap)
        val arr = arrMap.get(arrName).get
        val ind = eval(index, varMap, arrMap)
        arr(ind) = snd
        arrMap.update(arrName, arr)
        varMap.update(varName, fst)
      case SwapAA(arrName1, index1, arrName2, index2) =>
        val fst = eval(ArrElement(index1, arrName1), varMap, arrMap)
        val snd = eval(ArrElement(index2, arrName2), varMap, arrMap)
        val arr1 = arrMap.get(arrName1).get
        val arr2 = arrMap.get(arrName2).get
        val ind1 = eval(index1, varMap, arrMap)
        val ind2 = eval(index2, varMap, arrMap)
        arr1(ind1) = snd
        arr2(ind2) = fst
        arrMap.update(arrName1, arr1)
        arrMap.update(arrName2, arr2)
      case Loop(fromExp, block, untilExp) =>
        if (eval(fromExp, varMap, arrMap) == 0) raise(tree,"Loop enter condition is 0")
        while (eval(untilExp, varMap, arrMap) == 0){
          exec(block, varMap, procMap, arrMap)
          if (eval(fromExp, varMap, arrMap) != 0) raise(tree,"Loop condition should not be 0")
        }
      case Repeat(fromExp, block, untilExp) =>
        if (eval(fromExp, varMap, arrMap) == 0) raise(tree,"Loop enter condition is 0")
        do {
          exec(block, varMap, procMap, arrMap)
        }
        while (eval(untilExp, varMap, arrMap) == 0)
      case IfThen(ifcond, thenBlock, elseBlock, ficond) =>
        val ifCond = eval(ifcond, varMap, arrMap) != 0
        if (ifCond) {
          exec(thenBlock, varMap, procMap, arrMap)
        }else{
          exec(elseBlock, varMap, procMap, arrMap)
        }
        if ((eval(ficond, varMap, arrMap) != 0) != ifCond) {
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
  def collect(tree: ChemodanAST, varMap: Map[String, Int],
    procMap: Map[String, ChemodanAST], arrayMap: Map[String, Array[Int]]): Unit = {
    tree match {
      case AndThen(step1, step2) =>
        collect(step1, varMap, procMap, arrayMap);
        collect(step2, varMap, procMap, arrayMap)
      case Procedure(name, stat) => procMap.update(name, stat)
      case GlobalArray(name, size) => (arrayMap get name) match {
        case Some(_) => raise(tree, s"Array $name has already been defined")
        case None => arrayMap.update(name, Array.fill(size)(0))
      }
      case GlobalVar(varName) => (varMap get varName) match {
        case Some(_) => raise(tree,s"Variable $varName has already been defined")
        case None => varMap.update(varName, 0)
      }
      case _ => raise(tree,s"Statement outside procedure")
    }
  }

  // Pure function, evaluates expressions
  def eval(tree: Expression, varMap: Map[String, Int], arrMap: Map[String, Array[Int]]): Int = {
    tree match {
      case Number(value) => value
      case Variable(varName) => (varMap get varName) match {
        case Some(x) => x
        case None => throw new Error(s"Variable $varName is not defined")
      }
      case ArrElement(index, arrName) => (arrMap get arrName) match {
        case None => throw new Error(s"Array $arrName is not defined")
        case Some(lst) =>
          val ind = eval(index, varMap, arrMap)
          if(ind < lst.size && ind >= 0)
            lst(ind)
          else
            throw new Error(s"Index $ind is outside the range of array $arrName")
      }
      case Equals(exp1, exp2) => if(eval(exp1, varMap, arrMap) == eval(exp2, varMap, arrMap)) 1 else 0
      case Less(exp1, exp2)   => if(eval(exp1, varMap, arrMap) < eval(exp2, varMap, arrMap)) 1 else 0
      case Leq(exp1, exp2)   => if(eval(exp1, varMap, arrMap) <= eval(exp2, varMap, arrMap)) 1 else 0
      case Neq(exp1, exp2)   => if(eval(exp1, varMap, arrMap) != eval(exp2, varMap, arrMap)) 1 else 0
      case Plus(exp1, exp2)   => eval(exp1, varMap, arrMap) + eval(exp2, varMap, arrMap)
      case Minus(exp1, exp2)  => eval(exp1, varMap, arrMap) - eval(exp2, varMap, arrMap)
      case Mult(exp1, exp2)   => eval(exp1, varMap, arrMap) * eval(exp2, varMap, arrMap)
      case Div(exp1, exp2)    => eval(exp1, varMap, arrMap) / eval(exp2, varMap, arrMap)
      case Mod(exp1, exp2)    => eval(exp1, varMap, arrMap) % eval(exp2, varMap, arrMap)
    }
  }

  def zeroCheck(varMap: Map[String, Int], arrMap: Map[String, Array[Int]]) = {
    // Create an error message
    def compMsg(keys: List[String], values:List[Int]): String = {
      (keys, values) match {
        case (k::Nil, v::Nil) => s"$k = $v"
        case (k::ks, v::vs) => s"$k = $v, " + compMsg(ks, vs)
        case _ => ""
      }
    }
    val nonZero = varMap.filter({case (_, num) => num != 0})
    val nonZeroArr = arrMap.filter({ case (_, arr) => arr.exists({case (num) => num != 0})})
    val keys = nonZeroArr.keys.toList.mkString
    val str = compMsg(nonZero.keys.toList, nonZero.values.toList)
    if (nonZero.size > 0) {
      throw new Error(s"Nonzero variables at program end: {$str}")
    }
    if (nonZeroArr.size > 0) {
      throw new Error(s"Arrays with nonzero elements at program end {$keys}")
    }
  }

  // Try finding a variable in a given expression
  def treeSearch(tree: Expression, node: Expression): Boolean = {
    def search(exp1: Expression, exp2: Expression): Boolean = {
      treeSearch(exp1, node) || treeSearch(exp2, node)
    }
    // Ugly, but still the best way in Scala
    tree match {
      case Variable(name)     => Variable(name) == node
      case Equals(exp1, exp2) => search(exp1, exp2)
      case Less(exp1, exp2)   => search(exp1, exp2)
      case Plus(exp1, exp2)   => search(exp1, exp2)
      case Minus(exp1, exp2)  => search(exp1, exp2)
      case Mult(exp1, exp2)   => search(exp1, exp2)
      case Div(exp1, exp2)    => search(exp1, exp2)
      case Mod(exp1, exp2)    => search(exp1, exp2)
      case ArrElement(index, name) => ArrElement(index, name) == node
      case _ => false
    }
  }

  def varUpdate(varName: String, exp: Expression, varMap: Map[String, Int],
    func:(Int,Int)=>Int, arrMap: Map[String, Array[Int]]) = {
    treeSearch(exp, Variable(varName)) match {
      case false =>
        val prev = eval(Variable(varName), varMap, arrMap)
        val res = func(prev, eval(exp, varMap, arrMap))
        varMap.update(varName, res)
      case _ =>
        throw new Error(s"Value of $varName is used in its own update")
    }
  }
  def varUpdateArr(arrName: String, index: Expression, exp: Expression, varMap: Map[String, Int],
    func:(Int,Int)=>Int, arrMap: Map[String, Array[Int]]) = {
    val ind = eval(index, varMap, arrMap)
    treeSearch(exp, ArrElement(index, arrName)) match {
      case false =>
        val prev = eval(ArrElement(index, arrName), varMap, arrMap)
        val res = func(prev, eval(exp, varMap, arrMap))
        val arr = arrMap.get(arrName).get
        arr(ind) = res
        arrMap.update(arrName, arr)
      case _ =>
        throw new Error(s"Value of $arrName[$ind] is used in its own update")
    }
  }

  def interpret(tree: ChemodanAST) = {
    // val varMap = HashMap("w" -> 12, "j" -> 34, "k" -> 51)
    val varMap = HashMap.empty[String, Int]          // Mutable variable map
    val procMap = HashMap.empty[String, ChemodanAST] // Mutable procedure map
    val arrayMap = HashMap.empty[String, Array[Int]] // Mutable array map
    collect(tree, varMap, procMap, arrayMap)
    // println(procMap)
    // arrayMap.update("arr", Array(1,2,3,4,5))
    // println(arrayMap)
      (procMap get "main") match {
      case Some(main) => exec(main, varMap, procMap, arrayMap)
      case None => throw new Error("Main procedure can not be found")
    }
    zeroCheck(varMap, arrayMap)
  }
}
