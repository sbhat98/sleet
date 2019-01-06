package com.sleet.assembler

import com.sleet.runtime.Instruction
import com.sleet.runtime.LC3Instructions._

import scala.util.matching.Regex

object LC3InstructionPatterns {
  private val register: String = "R([0-7])"
  private val s: String = raw"\s*" // space
  private val number: String = raw"(X\d{4}|-?\d+)"

  private val addRegister: Regex = s"ADD $s$register$s,$s$register$s,$s$register".r
  private val addImmediate: Regex = s"ADD $s$register$s,$s$register$s,$s$number".r
  private val andRegisters: Regex = s"AND $s$register$s,$s$register$s,$s$register".r
  private val andImmediate: Regex = s"AND $s$register$s,$s$register$s,$s$number".r
  private val branch: Regex = s"BR(N?)(Z?)(P?) $s$number".r
  private val jump: Regex = s"JMP $s$register".r
  private val `return`: Regex = s"RET".r
  private val subroutineJumpFromOffset: Regex = s"JSR $s$number".r
  private val subroutineJumpFromRegister: Regex = s"JSRR $s$register".r
  private val load: Regex = s"LD $s$register$s,$s$number".r
  private val loadIndirect: Regex = s"LDI $s$register$s,$s$number".r
  private val loadBaseOffset: Regex = s"LDR $s$register$s,$s$register$s,$s$number".r
  private val loadEffectiveAddress: Regex = s"LEA $s$register$s,$s$number".r
  private val not: Regex = s"NOT $s$register$s,$s$register".r
  private val store: Regex = s"ST $s$register$s,$s$number".r
  private val storeIndirect: Regex = s"STI $s$register$s,$s$number".r
  private val storeBaseOffset: Regex = s"STR $s$register$s,$s$register$s,$s$number".r
  private val trap: Regex = s"TRAP $number".r

  private val getc: Regex = "GETC".r
  private val out: Regex = "OUT".r
  private val puts: Regex = "PUTS".r
  private val in: Regex = "IN".r
  private val putsp: Regex = "PUTSP".r
  private val halt: Regex = "HALT".r

  def toInstruction(line: String, label: Option[String]): Option[Instruction] = Option(line match {
    case addRegister(dest, s1, s2) =>
      AddRegisters(dest.toInt, s1.toInt, s2.toInt)
    case addImmediate(dest, reg, num) =>
      AddImmediate(dest.toInt, reg.toInt, numStr2Short(num, 5))
    case andRegisters(dest, s1, s2) =>
      AndRegisters(dest.toInt, s1.toInt, s2.toInt)
    case andImmediate(dest, reg, num) =>
      AndImmediate(dest.toInt, reg.toInt, numStr2Short(num, 5))
    case branch(n, z, p, offset) =>
      val N = n.nonEmpty
      val Z = z.nonEmpty
      val P = p.nonEmpty
      if (!N && !P && !Z) Branch(n = true, z = true, p = true, numStr2Short(offset, 9), label)
      else Branch(N, Z, P, numStr2Short(offset, 9), label)
    case jump(reg) =>
      Jump(reg.toInt)
    case `return` =>
      Return
    case subroutineJumpFromOffset(offset) =>
      SubroutineJumpFromOffset(numStr2Short(offset, 11), label)
    case subroutineJumpFromRegister(reg) =>
      SubroutineJumpFromRegister(reg.toInt)
    case load(reg, offset) =>
      Load(reg.toInt, numStr2Short(offset, 9), label)
    case loadIndirect(reg, offset) =>
      LoadIndirect(reg.toInt, numStr2Short(offset, 9), label)
    case loadBaseOffset(reg1, reg2, offset) =>
      LoadBaseOffset(reg1.toInt, reg2.toInt, numStr2Short(offset, 6))
    case loadEffectiveAddress(reg, offset) =>
      LoadEffectiveAddress(reg.toInt, numStr2Short(offset, 9), label)
    case not(reg1, reg2) =>
      Not(reg1.toInt, reg2.toInt)
    case store(reg, offset) =>
      Store(reg.toInt, numStr2Short(offset, 9), label)
    case storeIndirect(reg, offset) =>
      StoreIndirect(reg.toInt, numStr2Short(offset, 9), label)
    case storeBaseOffset(reg1, reg2, offset) =>
      StoreBaseOffset(reg1.toInt, reg2.toInt, numStr2Short(offset, 6))
    case trap(pc) =>
      val offset = numStr2Short(pc, 9)
      if (offset < 0) throw new ValueOutOfBounds(pc)
      else Trap(offset)
    case getc() =>
      GetC
    case out() =>
      Out
    case puts() =>
      Puts
    case in() =>
      In
    case putsp() =>
      PutsP
    case halt() =>
      Halt
    case _ =>
      null
  })
}
