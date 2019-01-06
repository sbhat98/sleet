package com.sleet

package object assembler {
  val numStr2Short: (String, Int) => Short = (num, maxBinaryLength) => {
    val value =
      try {
        if (num.startsWith("X")) Integer.parseInt(num.tail, 16)
        else Integer.parseInt(num)
      } catch {
        case _: NumberFormatException => throw new ValueOutOfBounds(num)
      }
    if (value < -Math.pow(2, maxBinaryLength - 1).toInt || value >= Math.pow(2, maxBinaryLength - 1).toInt)
      throw new ValueOutOfBounds(num)
    else
      value.toShort
  }
}
