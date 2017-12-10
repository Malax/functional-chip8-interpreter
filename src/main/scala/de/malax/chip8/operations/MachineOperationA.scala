package de.malax.chip8.operations

import de.malax.chip8.{Key, Register}
import scodec.bits.ByteVector

sealed trait MachineOperationA[A]
case class PatchMemory(address: Int, bytes: ByteVector) extends MachineOperationA[Unit]
case object ClearScreen extends MachineOperationA[Unit]
case class PushStack(address: Int) extends MachineOperationA[Unit]
case object PopStack extends MachineOperationA[Int]
case object GenerateRandom extends MachineOperationA[Int]
case class DrawSprite(x: Int, y: Int, bytes: ByteVector) extends MachineOperationA[Boolean]
case class GetKeyDown(key: Key) extends MachineOperationA[Boolean]
case class ReadByte(address: Int) extends MachineOperationA[Int]
case class WriteByte(address: Int, value: Int) extends MachineOperationA[Unit]
case class ReadRegister(register: Register) extends MachineOperationA[Int]
case class WriteRegister(register: Register, byte: Int) extends MachineOperationA[Unit]
case object ReadPc extends MachineOperationA[Int]
case class WritePc(address: Int) extends MachineOperationA[Unit]
case object ReadPowerState extends MachineOperationA[Boolean]
