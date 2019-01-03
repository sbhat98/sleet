package com.sleet.assembler

import com.sleet.runtime.{Instruction, State}

object Assembler {
  def assemble(source: Traversable[String]): State = ???

  def toInstruction(line: Short): Instruction = ???

  def toInstruction(line: String): Instruction = ???
}
