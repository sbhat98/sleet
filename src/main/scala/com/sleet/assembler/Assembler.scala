package com.sleet.assembler

import com.sleet.runtime.{Instruction, State}
import scala.language.implicitConversions
import util.control.Breaks._

object Assembler {

  private type SymbolTable = Map[String, Int]

  def assemble(source: Seq[String]): State = {
    val lines = source.view
      .map(stripComments)
      .map(_.trim.toUpperCase)
      .zip(source.indices.map(_ + 1))
      .filter{case (line, _) => line.nonEmpty}.force
    ???
  }

  def toInstruction(line: Short): Instruction = ???

  def toInstruction(line: String, label: Option[String]): Option[Instruction] =
    LC3InstructionPatterns.toInstruction(line, label)

  // Returns the constructed symbol table and the code stripped of prefix labels
  private def buildSymbolTable(source: Seq[(String, Int)]): (SymbolTable, Seq[String]) = {
    var table: SymbolTable = Map()
    var strippedLines: List[(String, Int)] = Nil
    var currentPc: Option[Int] = None

    val orig = raw"\.ORIG\s*(X\d{4}|-?\d+)".r

    for ((line, lineNum) <- source) {
      breakable {
        if (currentPc.isEmpty) {
          line match {
            case orig(num) =>
              currentPc = Some(numStr2Short(num, 16))
            case _ => throw new SyntaxError("Expected a .orig directive on line" + lineNum)
          }
          break
        } else if (line.matches(raw"\.END")) {
          currentPc = None
          break
        }
        val currPc = currentPc.get
        if (line.matches())
      }
    }

    ???
  }

  private def stripComments(line: String): String = line.takeWhile(_ != ';')
}
