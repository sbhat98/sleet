package com.sleet.runtime

import com.sleet.assembler.Assembler

import scala.util.Random

object Memory {
  val MemorySize: Int = 0xFFFF

  def initialize(creationFunction: Int => Instruction): Memory =
    Memory(Vector.tabulate(Memory.MemorySize)(creationFunction))
  def initializeRandom: Memory =
    initialize(_ => Assembler.toInstruction(Random.nextInt(Instruction.InstructionSize)))
}

case class Memory(memory: Vector[Instruction]) {
  def updated(index: Short, instruction: Instruction): Memory =
    if (index < 0 || index >= Memory.MemorySize) throw new SegmentationFault
    else this.copy(memory = memory.updated(index, instruction))

  def apply(index: Short): Instruction =
    if (index < 0 || index >= Memory.MemorySize) throw new SegmentationFault
    else memory(index)
}
