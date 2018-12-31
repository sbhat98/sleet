package com.sleet.assembler

import com.sleet.runtime.State

abstract sealed class Instruction {
  def execute(state: State): State
  val toBinary: String

  private val toBinaryString: (Int, Int) => String = (value, len) =>
    value.toBinaryString.reverse.padTo(len, '0').reverse.takeRight(len)
  protected val regToBinary: Int => String = toBinaryString(_, 3)
  protected val immedToBinary: Int => String = toBinaryString(_, 5)
  protected val offset6ToBinary: Int => String = toBinaryString(_, 6)
  protected val offset8ToBinary: Int => String = toBinaryString(_, 8)
  protected val offset9ToBinary: Int => String = toBinaryString(_, 9)
  protected val offset11ToBinary: Int => String = toBinaryString(_, 11)
  protected val booleanToBinary: Boolean => String = if (_) "1" else "0"
}

case class AddRegisters(directRegister: Int, sourceRegister1: Int, sourceRegister2: Int) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "0001" + regToBinary(directRegister) + regToBinary(sourceRegister1) + "000" + regToBinary(sourceRegister2)
  override val toString: String = s"ADD R$directRegister, R$sourceRegister1, R$sourceRegister2"
}

case class AddImmediate(directRegister: Int, sourceRegister: Int, immediate: Int) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "0001" + regToBinary(directRegister) + regToBinary(sourceRegister) + "1" + immedToBinary(immediate)
  override def toString: String = s"ADD R$directRegister, R$sourceRegister, #$immediate"
}

case class AndRegisters(directRegister: Int, sourceRegister1: Int, sourceRegister2: Int) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "0101" + regToBinary(directRegister) + regToBinary(sourceRegister1) + "000" + regToBinary(sourceRegister2)
  override val toString: String = s"AND R$directRegister, R$sourceRegister1, R$sourceRegister2"
}
case class AndImmediate(directRegister: Int, sourceRegister: Int, immediate: Int) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "0101" + regToBinary(directRegister) + regToBinary(sourceRegister) + "1" + immedToBinary(immediate)
  override def toString: String = s"AND R$directRegister, R$sourceRegister, #$immediate"
}

case class Branch(n: Boolean, z: Boolean, p: Boolean, pcOffset9: Int, label: Option[String]) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "0000" + booleanToBinary(n) + booleanToBinary(z) + booleanToBinary(p) + offset9ToBinary(pcOffset9)
  override def toString: String =
    s"BR${if (n) "n" else ""}${if (z) "z" else ""}${if (p) "p" else ""} ${label.getOrElse("#" + pcOffset9)}"
}

case class Jump(baseRegister: Int) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "1100000" + regToBinary(baseRegister) + "000000"
  override def toString: String =
    "JMP R" + baseRegister
}

case object Return extends Jump(7) {
  override def toString: String = "RET"
}

case class SubroutineJumpFromOffset(pcOffset11: Int, label: Option[String]) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "01001" + offset11ToBinary(pcOffset11)
  override def toString: String =
    "JSR " + label.getOrElse("#" + pcOffset11)
}

case class SubroutineJumpFromRegister(baseRegister: Int) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "0100000" + regToBinary(baseRegister) + "000000"
  override def toString: String =
    "JSRR R" + baseRegister
}

case class Load(directRegister: Int, pcOffset9: Int, label: Option[String]) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "0010" + regToBinary(directRegister) + offset9ToBinary(pcOffset9)
  override def toString: String =
    s"LD R$directRegister, ${label.getOrElse("#" + pcOffset9)}"
}

case class LoadIndirect(directRegister: Int, pcOffset9: Int, label: Option[String]) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "1010" + regToBinary(directRegister) + offset9ToBinary(pcOffset9)
  override def toString: String =
    s"LDI R$directRegister, ${label.getOrElse("#" + pcOffset9)}"
}

case class LoadBaseOffset(directRegister: Int, baseRegister: Int, offset6: Int) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "0110" + regToBinary(directRegister) + regToBinary(baseRegister) + offset6ToBinary(offset6)
  override def toString: String =
    s"LDR R$directRegister, R$baseRegister, #$offset6"
}

case class LoadEffectiveAddress(directRegister: Int, pcOffset9: Int, label: Option[String]) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "1110" + regToBinary(directRegister) + offset9ToBinary(pcOffset9)
  override def toString: String =
    s"LEA R$directRegister, ${label.getOrElse("#" + pcOffset9)}"
}

case class Not(directRegister: Int, sourceRegister: Int) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "1001" + regToBinary(directRegister) + regToBinary(sourceRegister) + "111111"
  override def toString: String =
    s"NOT R$directRegister, R$sourceRegister"
}

// TODO RTI

case class Store(sourceRegister: Int, pcOffset9: Int, label: Option[String]) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "0011" + regToBinary(sourceRegister) + offset9ToBinary(pcOffset9)
  override def toString: String =
    s"ST R$sourceRegister, ${label.getOrElse("#" + pcOffset9)}"
}

case class StoreIndirect(sourceRegister: Int, pcOffset9: Int, label: Option[String]) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "0011" + regToBinary(sourceRegister) + offset9ToBinary(pcOffset9)
  override def toString: String =
    s"ST R$sourceRegister, ${label.getOrElse("#" + pcOffset9)}"
}

case class StoreBaseOffset(sourceRegister: Int, baseRegister: Int, offset6: Int) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "0111" + regToBinary(sourceRegister) + regToBinary(baseRegister) + offset6ToBinary(offset6)
  override def toString: String =
    s"STR R$sourceRegister, R$baseRegister, #$offset6"
}

case class Trap(trapvect8: Int) extends Instruction {
  def execute(state: State): State = ???
  val toBinary: String =
    "11110000" + offset8ToBinary(trapvect8)
  override def toString: String =
    "TRAP 0x" + trapvect8.toHexString
}