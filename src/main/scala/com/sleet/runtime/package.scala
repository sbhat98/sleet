package com.sleet

import language.implicitConversions

package object runtime {
  private val toBinaryString: (Int, Int) => String = (value, len) =>
    value.toBinaryString.reverse.padTo(len, '0').reverse.takeRight(len)
  val regToBinary: Int => String = toBinaryString(_, 3)
  val immedToBinary: Int => String = toBinaryString(_, 5)
  val offset6ToBinary: Int => String = toBinaryString(_, 6)
  val offset8ToBinary: Int => String = toBinaryString(_, 8)
  val offset9ToBinary: Int => String = toBinaryString(_, 9)
  val offset11ToBinary: Int => String = toBinaryString(_, 11)
  val shortToBinary: Int => String = toBinaryString(_, 16)
  val booleanToBinary: Boolean => String = if (_) "1" else "0"

  val getCC: Short => Char = value =>
    if (value == 0) 'z'
    else if (value > 0) 'p'
    else 'n'

  implicit def int2Short(x: Int): Short = x.toShort
}
