package com.sleet.runtime

import com.sleet.assembler.Assembler

case class NoOperation(value: Short) extends Instruction {
  def execute(state: State): State = throw new SegmentationFault
  val toBinary: String = shortToBinary(value)
  override def toString: String = "NOP"
}

case class AddRegisters(directRegister: Int, sourceRegister1: Int, sourceRegister2: Int) extends Instruction {
  def execute(state: State): State = {
    val s1 = state.registers(sourceRegister1)
    val s2 = state.registers(sourceRegister2)
    val res = s1 + s2
    state.copy(
      registers = state.registers.updated(directRegister, res),
      pcCounter = state.pcCounter + 1,
      conditionCode = getCC(res)
    )
  }
  val toBinary: String =
    "0001" + regToBinary(directRegister) + regToBinary(sourceRegister1) + "000" + regToBinary(sourceRegister2)
  override val toString: String = s"ADD R$directRegister, R$sourceRegister1, R$sourceRegister2"
}

case class AddImmediate(directRegister: Int, sourceRegister: Int, immediate: Short) extends Instruction {
  def execute(state: State): State = {
    val s = state.registers(sourceRegister)
    val res = s + immediate
    state.copy(
      registers = state.registers.updated(directRegister, res),
      pcCounter = state.pcCounter + 1,
      conditionCode = getCC(res)
    )
  }
  val toBinary: String =
    "0001" + regToBinary(directRegister) + regToBinary(sourceRegister) + "1" + immedToBinary(immediate)
  override def toString: String = s"ADD R$directRegister, R$sourceRegister, #$immediate"
}

case class AndRegisters(directRegister: Int, sourceRegister1: Int, sourceRegister2: Int) extends Instruction {
  def execute(state: State): State = {
    val s1 = state.registers(sourceRegister1)
    val s2 = state.registers(sourceRegister2)
    val res = s1 & s2
    state.copy(
      registers = state.registers.updated(directRegister, res),
      pcCounter = state.pcCounter + 1,
      conditionCode = getCC(res)
    )
  }
  val toBinary: String =
    "0101" + regToBinary(directRegister) + regToBinary(sourceRegister1) + "000" + regToBinary(sourceRegister2)
  override val toString: String = s"AND R$directRegister, R$sourceRegister1, R$sourceRegister2"
}
case class AndImmediate(directRegister: Int, sourceRegister: Int, immediate: Short) extends Instruction {
  def execute(state: State): State = {
    val s = state.registers(sourceRegister)
    val res: Short = s & immediate
    state.copy(
      registers = state.registers.updated(directRegister, res),
      pcCounter = state.pcCounter + 1,
      conditionCode = getCC(res)
    )
  }
  val toBinary: String =
    "0101" + regToBinary(directRegister) + regToBinary(sourceRegister) + "1" + immedToBinary(immediate)
  override def toString: String = s"AND R$directRegister, R$sourceRegister, #$immediate"
}

case class Branch(n: Boolean, z: Boolean, p: Boolean, pcOffset9: Short, label: Option[String]) extends Instruction {
  def execute(state: State): State = {
    val branchedState = state.copy(pcCounter = state.pcCounter + 1 - pcOffset9)
    state.conditionCode match {
      case 'z' if z => branchedState
      case 'n' if n => branchedState
      case 'p' if p => branchedState
      case _ => state.copy(pcCounter = state.pcCounter + 1)
    }
  }
  val toBinary: String =
    "0000" + booleanToBinary(n) + booleanToBinary(z) + booleanToBinary(p) + offset9ToBinary(pcOffset9)
  override def toString: String =
    s"BR${if (n) "n" else ""}${if (z) "z" else ""}${if (p) "p" else ""} ${label.getOrElse("#" + pcOffset9)}"
}

case class Jump(baseRegister: Int) extends Instruction {
  def execute(state: State): State = {
    val b = state.registers(baseRegister)
    state.copy(
      pcCounter = b
    )
  }
  val toBinary: String =
    "1100000" + regToBinary(baseRegister) + "000000"
  override def toString: String =
    "JMP R" + baseRegister
}

object Return extends Jump(7) {
  override def toString: String = "RET"
}

case class SubroutineJumpFromOffset(pcOffset11: Short, label: Option[String]) extends Instruction {
  def execute(state: State): State = {
    state.copy(
      registers = state.registers.updated(7, state.pcCounter + 1),
      pcCounter = state.pcCounter + 1 + pcOffset11
    )
  }
  val toBinary: String =
    "01001" + offset11ToBinary(pcOffset11)
  override def toString: String =
    "JSR " + label.getOrElse("#" + pcOffset11)
}

case class SubroutineJumpFromRegister(baseRegister: Int) extends Instruction {
  def execute(state: State): State = {
    state.copy(
      registers = state.registers.updated(7, state.pcCounter + 1),
      pcCounter = state.registers(baseRegister)
    )
  }
  val toBinary: String =
    "0100000" + regToBinary(baseRegister) + "000000"
  override def toString: String =
    "JSRR R" + baseRegister
}

case class Load(directRegister: Int, pcOffset9: Short, label: Option[String]) extends Instruction {
  def execute(state: State): State = {
    val pc = state.pcCounter + 1
    val m = state.memory(pc + pcOffset9).shortValue
    state.copy(
      registers = state.registers.updated(directRegister, m),
      pcCounter = pc,
      conditionCode = getCC(m),
    )
  }
  val toBinary: String =
    "0010" + regToBinary(directRegister) + offset9ToBinary(pcOffset9)
  override def toString: String =
    s"LD R$directRegister, ${label.getOrElse("#" + pcOffset9)}"
}

case class LoadIndirect(directRegister: Int, pcOffset9: Short, label: Option[String]) extends Instruction {
  def execute(state: State): State = {
    val pc = state.pcCounter + 1
    val direct = state.memory(pc + pcOffset9).shortValue
    val res = state.memory(direct).shortValue
    state.copy(
      registers = state.registers.updated(directRegister, res),
      pcCounter = pc,
      conditionCode = getCC(res)
    )
  }
  val toBinary: String =
    "1010" + regToBinary(directRegister) + offset9ToBinary(pcOffset9)
  override def toString: String =
    s"LDI R$directRegister, ${label.getOrElse("#" + pcOffset9)}"
}

case class LoadBaseOffset(directRegister: Int, baseRegister: Int, offset6: Short) extends Instruction {
  def execute(state: State): State = {
    val b = state.registers(baseRegister)
    val res = state.memory(b + offset6).shortValue
    state.copy(
      registers = state.registers.updated(directRegister, res),
      pcCounter = state.pcCounter + 1,
      conditionCode = getCC(res)
    )
  }
  val toBinary: String =
    "0110" + regToBinary(directRegister) + regToBinary(baseRegister) + offset6ToBinary(offset6)
  override def toString: String =
    s"LDR R$directRegister, R$baseRegister, #$offset6"
}

case class LoadEffectiveAddress(directRegister: Int, pcOffset9: Short, label: Option[String]) extends Instruction {
  def execute(state: State): State = {
    val pc = state.pcCounter + 1
    val res = pc + pcOffset9
    state.copy(
      registers = state.registers.updated(directRegister, res),
      pcCounter = pc,
      conditionCode = getCC(res)
    )
  }
  val toBinary: String =
    "1110" + regToBinary(directRegister) + offset9ToBinary(pcOffset9)
  override def toString: String =
    s"LEA R$directRegister, ${label.getOrElse("#" + pcOffset9)}"
}

case class Not(directRegister: Int, sourceRegister: Int) extends Instruction {
  def execute(state: State): State = {
    val s = state.registers(sourceRegister)
    val res = ~s
    state.copy(
      registers = state.registers.updated(directRegister, res),
      pcCounter = state.pcCounter + 1,
      conditionCode = getCC(res)
    )
  }
  val toBinary: String =
    "1001" + regToBinary(directRegister) + regToBinary(sourceRegister) + "111111"
  override def toString: String =
    s"NOT R$directRegister, R$sourceRegister"
}

// TODO RTI

case class Store(sourceRegister: Int, pcOffset9: Short, label: Option[String]) extends Instruction {
  def execute(state: State): State = {
    val s = state.registers(sourceRegister)
    val pc = state.pcCounter + 1
    state.copy(
      pcCounter = pc,
      memory = state.memory.updated(pc + pcOffset9, Assembler.toInstruction(s))
    )
  }
  val toBinary: String =
    "0011" + regToBinary(sourceRegister) + offset9ToBinary(pcOffset9)
  override def toString: String =
    s"ST R$sourceRegister, ${label.getOrElse("#" + pcOffset9)}"
}

case class StoreIndirect(sourceRegister: Int, pcOffset9: Short, label: Option[String]) extends Instruction {
  def execute(state: State): State = {
    val s = state.registers(sourceRegister)
    val pc = state.pcCounter + 1
    val location = state.memory(pc + pcOffset9).shortValue
    state.copy(
      pcCounter = pc,
      memory = state.memory.updated(location, Assembler.toInstruction(s))
    )
  }
  val toBinary: String =
    "0011" + regToBinary(sourceRegister) + offset9ToBinary(pcOffset9)
  override def toString: String =
    s"ST R$sourceRegister, ${label.getOrElse("#" + pcOffset9)}"
}

case class StoreBaseOffset(sourceRegister: Int, baseRegister: Int, offset6: Short) extends Instruction {
  def execute(state: State): State = {
    val b = state.registers(baseRegister)
    val s = state.registers(sourceRegister)
    val location = state.memory(b + offset6).shortValue
    state.copy(
      pcCounter = state.pcCounter + 1,
      memory = state.memory.updated(location, Assembler.toInstruction(s))
    )
  }
  val toBinary: String =
    "0111" + regToBinary(sourceRegister) + regToBinary(baseRegister) + offset6ToBinary(offset6)
  override def toString: String =
    s"STR R$sourceRegister, R$baseRegister, #$offset6"
}

case class Trap(trapvect8: Short) extends Instruction {
  def execute(state: State): State = {
    state.copy(
      registers = state.registers.updated(7, state.pcCounter + 1),
      pcCounter = state.memory(trapvect8).shortValue
    )
  }
  val toBinary: String =
    "11110000" + offset8ToBinary(trapvect8)
  override def toString: String =
    "TRAP 0x" + trapvect8.toHexString
}