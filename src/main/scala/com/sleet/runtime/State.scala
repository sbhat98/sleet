package com.sleet.runtime

case class State(registers: RegisterSet, pcCounter: Int, conditionCode: Char)