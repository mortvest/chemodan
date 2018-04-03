package chemodan

import chemodan.interpreter.{Location, ChemodanInterpreter, ChemodanParserError}
import chemodan.parser._
import org.rogach.scallop._
import scala.io.Source

// Setting up command line arguments
class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val rev = opt[Boolean](descr = "Run program backwards")
  val file = trailArg[String](required = true)
  verify()
}

object ChemodanMain{
  def printLogo (fileName: String, reverse: Boolean){
//     print("""
// ********************************************************
// **                                                    **
// **     #   # ##### #   #  ###   ####    #   #   #     **
// **     #   # #     ## ## #   #  #  #   # #  #   #     **
// **      #### ####  # # # #   #  #  #  ##### #####     **
// **         # #     #   # #   #  ####  #   # #   #     **
// **         # ##### #   #  ###  #    # #   # #   #     **
// **                                                    **
// ********************************************************
// """)
    if (reverse) {
      println(s"> $fileName <<< RUNNING BACKWARDS")
    }else{
      println(s"> $fileName >>> RUNNING FORWARDS")
    }
    println()
  }

  def main(args: Array[String]) = {
    val dirPattern = ".+.che".r
    val conf = new Conf(args)
    dirPattern.findFirstIn(conf.file()) match {
      case Some(dir) =>
        val fileContent = Source.fromFile(conf.file()).getLines.mkString("\n")
        conf.rev() match {
          case false =>
            printLogo(dir, false)
            ChemodanInterpreter(fileContent, false)
          case _ =>
            printLogo(dir, true)
            ChemodanInterpreter(fileContent, true)
        }
      case None => println("Please provide a .che file")
    }
  }
  // val validCode =
  //   """
  //   |global x
  //   |
  //   |procedure main
  //   |    read x
  //   |    print x
  // """.stripMargin.trim
  // ChemodanInterpreter(validCode)
}
