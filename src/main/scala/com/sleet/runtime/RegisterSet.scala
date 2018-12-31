package com.sleet.runtime

object RegisterSet {
  val MinimumRegisterValue: Int = -Math.pow(2, 15).toInt
  val MaximumRegisterValue: Int = Math.pow(2, 15).toInt - 1
}

class RegisterSet(initialVals: Vector[Int]) {
  require(initialVals.size == 7)
  require(initialVals.forall(validate))

  private def validate(value: Int): Boolean =
    RegisterSet.MinimumRegisterValue <= value && value <= RegisterSet.MaximumRegisterValue

  private val registers: Vector[Int] = initialVals

  def apply(register: Int): Int = {
    require(register >= 0 && register <= 7)
    registers(register)
  }

  def updated(register: Int, value: Int): RegisterSet = {
    require(register >= 0 && register <= 7)
    new RegisterSet(registers.updated(register, value))
  }
}
