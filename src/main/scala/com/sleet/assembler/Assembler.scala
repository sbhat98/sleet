package com.sleet.assembler

object Assembler {
  def assemble(source: Traversable[String]): Traversable[Instruction] = ???

  private def toInstruction(line: String): Instruction = ???
}
