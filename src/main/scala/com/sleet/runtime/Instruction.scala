package com.sleet.runtime

object Instruction {
  val InstructionSize = 0xFFFF
}

abstract class Instruction {
  def execute(state: State): State
  val toBinary: String
  lazy val shortValue: Short = Integer.parseInt(toBinary, 2)
}