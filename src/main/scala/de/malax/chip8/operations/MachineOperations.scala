package de.malax.chip8.operations

import cats.free.Free.liftF
import de.malax.chip8.{Key, Register}
import scodec.bits.ByteVector

object MachineOperations {
  def patchMemory(address: Int, bytes: ByteVector): MachineOperation[Unit] = liftF(PatchMemory(address, bytes))
  val clearScreen: MachineOperation[Unit] = liftF(ClearScreen)
  def pushStack(address: Int): MachineOperation[Unit] = liftF(PushStack(address))
  val popStack: MachineOperation[Int] = liftF(PopStack)
  val generateRandom: MachineOperation[Int] = liftF(GenerateRandom)
  def drawSprite(x: Int, y: Int, bytes: ByteVector): MachineOperation[Boolean] = liftF(DrawSprite(x, y, bytes))
  def getKeyDown(key: Key): MachineOperation[Boolean] = liftF(GetKeyDown(key))
  def readByte(address: Int): MachineOperation[Int] = liftF(ReadByte(address))
  def writeByte(address: Int, byte: Int): MachineOperation[Unit] = liftF(WriteByte(address, byte))
  def readRegister(register: Register): MachineOperation[Int] = liftF(ReadRegister(register))
  def writeRegister(register: Register, byte: Int): MachineOperation[Unit] = liftF(WriteRegister(register, byte))
  val readPc: MachineOperation[Int] = liftF(ReadPc)
  def writePc(address: Int): MachineOperation[Unit] = liftF(WritePc(address))
  val readPowerState: MachineOperation[Boolean] = liftF(ReadPowerState)

  def incrementProgramCounter(by: Int = 2): MachineOperation[Unit] = readPc.flatMap(pc => writePc(pc + by))
}
