package com.sleet.runtime

case class RegisterSet(registers: Vector[Short]) {
  require(registers.size == 7)

  def apply(register: Int): Short = {
    require(register >= 0 && register <= 7)
    registers(register)
  }

  def updated(register: Int, value: Short): RegisterSet = {
    require(register >= 0 && register <= 7)
    RegisterSet(registers.updated(register, value))
  }
}
