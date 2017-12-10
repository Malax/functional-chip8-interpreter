package de.malax.chip8.opcode

import de.malax.chip8.Register

sealed trait Opcode

case object ClearScreen extends Opcode
case object Return extends Opcode
case class JumpToAddress(address: Int) extends Opcode
case class CallSubroutine(address: Int) extends Opcode
case class SkipIfEqual(register: Register, byte: Int) extends Opcode
case class SkipIfNotEqual(register: Register, byte: Int) extends Opcode
case class SkipIfEqualRegister(registerA: Register, registerB: Register) extends Opcode
case class LoadConstant(register: Register, byte: Int) extends Opcode
case class AddConstant(register: Register, byte: Int) extends Opcode
case class LoadRegister(registerA: Register, registerB: Register) extends Opcode
case class OrRegister(registerA: Register, registerB: Register) extends Opcode
case class AndRegister(registerA: Register, registerB: Register) extends Opcode
case class XorRegister(registerA: Register, registerB: Register) extends Opcode
case class AddRegister(registerA: Register, registerB: Register) extends Opcode
case class SubtractRegister(registerA: Register, registerB: Register) extends Opcode
case class ShiftRight(rA: Register, rB: Register) extends Opcode
case class SubN(registerA: Register, registerB: Register) extends Opcode
case class ShiftLeft(aA: Register, rB: Register) extends Opcode
case class SkipIfNotEqualRegister(registerA: Register, registerB: Register) extends Opcode
case class LoadIRegister(byte: Int) extends Opcode
case class JumpToAddressOffsettedByVa(address: Int) extends Opcode
case class GenerateRandom(register: Register, mask: Int) extends Opcode
case class DrawSprite(registerX: Register, registerY: Register, sizeInBytes: Int) extends Opcode
case class SkipIfKeyPressed(register: Register) extends Opcode
case class SkipIfKeyNotPressed(register: Register) extends Opcode
case class ReadTimerValue(register: Register) extends Opcode
case class LoadPressedKey(register: Register) extends Opcode
case class LoadTimer(register: Register) extends Opcode
case class LoadSoundTimer(register: Register) extends Opcode
case class AddRegisterValueToI(register: Register) extends Opcode
case class LoadSpriteAddressForDigit(register: Register) extends Opcode
case class BinaryCodedDecimal(register: Register) extends Opcode
case class WriteRegistersToMemory(lastRegister: Register) extends Opcode
case class ReadRegistersFromMemory(lastRegister:Register) extends Opcode
