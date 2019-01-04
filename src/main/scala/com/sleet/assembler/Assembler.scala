package com.sleet.assembler

import com.sleet.runtime.{Instruction, State}
import scala.language.implicitConversions

object Assembler {

  private type SymbolTable = Map[String, Int]

  def assemble(source: Iterable[String]): State = {
    val lines = source.view
      .map(stripComments)
      .map(_.trim.toUpperCase)
      .filterNot(_.isEmpty).force
    ???
  }

  def toInstruction(line: Short): Instruction = ???

  def toInstruction(line: String, label: Option[String]): Option[Instruction] =
    LC3InstructionPatterns.toInstruction(line, label)

  // Returns the constructed symbol table and the code stripped of prefix labels
  private def buildSymbolTable(source: Iterable[String]): (SymbolTable, Iterable[String]) = {
    implicit def int2Short(x: Int): Short = x.toShort
    ???
  }

  private def stripComments(line: String): String = line.takeWhile(_ != ';')
}
