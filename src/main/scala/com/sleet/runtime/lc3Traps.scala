package com.sleet.runtime

import scala.io.StdIn

case object GetCharacter extends Instruction {
  def execute(state: State): State = {
    val character = StdIn.readChar()
    state.copy(
      pcCounter = state.registers(7),
      registers = state.registers.updated(0, character.toByte)
    )
  }
  val toBinary: String = "0000010100000011"
  override def toString: String = "GETC"
}

case object Out extends Instruction {
  override def execute(state: State): State = {
    val character = state.registers(0).toByte.toChar
    print(character)
    state.copy(
      pcCounter = state.registers(7)
    )
  }
  val toBinary: String = "0000010100000011"
  override def toString: String = "OUT"
}

case object Puts extends Instruction {
  def execute(state: State): State = {
    var currAddress = state.registers(0)
    while (state.memory(currAddress).shortValue != 0) {
      print(state.memory(currAddress).shortValue.toByte.toChar)
      currAddress += 1
    }
    state.copy(
      pcCounter = state.registers(7)
    )
  }
  val toBinary: String = "0000010100001101"
  override def toString: String = "PUTS"
}

case object In extends Instruction {
  def execute(state: State): State = {
    print("Enter a character: ")
    val char = StdIn.readChar()
    state.copy(
      registers = state.registers.updated(0, char.toByte),
      pcCounter = state.registers(7)
    )
  }
  val toBinary: String = "0000010100011010"
  override def toString: String = "IN"
}

case object PutsP extends Instruction {
  def execute(state: State): State = {
    var currAddress = state.registers(0)
    while (state.memory(currAddress).shortValue != 0) {
      val memVal = state.memory(currAddress).shortValue
      print(memVal.toByte.toChar)
      if ((memVal >> 8) != 0)
        print((memVal >> 8).toChar)
      currAddress += 1
    }
    state.copy(
      pcCounter = state.registers(7)
    )
  }
  val toBinary: String = "0000010100101000"
  override def toString: String = "PUTSP"
}

case object Halt extends Instruction {
  def execute(state: State): State = throw new ExecutionHalted
  val toBinary: String = "0000010101001001"
  override def toString: String = "HALT"
}