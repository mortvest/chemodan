// ********************************************************
// **                                                    **
// **     #   # ##### #   #  ###   ####    #   #   #     **
// **     #   # #     ## ## #   #  #  #   # #  #   #     **
// **      #### ####  # # # #   #  #  #  ##### #####     **
// **         # #     #   # #   #  ####  #   # #   #     **
// **         # ##### #   #  ###  #    # #   # #   #     **
// **                                                    **
// ********************************************************
package chemodan

import chemodan.interpreter.{Location, ChemodanInterpreter, ChemodanParserError}
import chemodan.parser._
import org.rogach.scallop._
import scala.io.Source

// Setting up command line arguments
class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val rev = opt[Boolean](descr = "Run program backwards")
  val debug = opt[Boolean](descr = "Debug mode")
  val file = trailArg[String](required = true)
  verify()
}

object ChemodanMain{
  def printLogo (fileName: String, reverse: Boolean){
    if (reverse) {
      println(s"USER PROGRAM <<< $fileName <<< RUNNING BACKWARDS")
    }else{
      println(s"USER PROGRAM >>> $fileName >>> RUNNING FORWARDS")
    }
    println()
  }

  def main(args: Array[String]) = {
    val dirPattern = ".+.che".r
    val conf = new Conf(args)
    dirPattern.findFirstIn(conf.file()) match {
      case Some(dir) =>
        val fileContent = Source.fromFile(conf.file()).getLines.mkString("\n")
        printLogo(dir, conf.rev())
        ChemodanInterpreter(fileContent, conf.rev(), conf.debug())
      case None => println("Please provide a .che file")
    }
  }
}
