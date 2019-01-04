package com.sleet

package object assembler {
  def numStr2Short(num: String, maxBinaryLength: Int): Short = {
    val value =
      if (num.startsWith("X")) Integer.parseInt(num.tail, 16)
      else Integer.parseInt(num)
    if (value < -Math.pow(2, maxBinaryLength).toInt || value >= Math.pow(2, maxBinaryLength).toInt)
      throw new ValueOutOfBounds(num)
    else
      value.toShort
  }
}
