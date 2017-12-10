package de.malax.chip8.compilers.pure

import cats.data.State
import cats.~>
import de.malax.chip8.operations._
import de.malax.chip8.utils.DisplayUtils
import scodec.bits.{BitVector, ByteVector}

import scala.util.Random

object PureCompiler {
  type MachineState[A] = State[Machine, A]

  val compiler: MachineOperationA ~> MachineState = new (MachineOperationA ~> MachineState) {
    override def apply[A](fa: MachineOperationA[A]) = {
      fa match {
        case PatchMemory(address, bytes) => State.modify[Machine](m => m.copy(memory = m.memory.patch(address, bytes)))
        case ClearScreen => State.modify[Machine](m => m.copy(display = BitVector.fill(m.display.length)(high = false)))
        case PushStack(address) => State.modify[Machine](m => m.copy(stack = address :: m.stack))
        case PopStack => State { m =>
          val head :: tail = m.stack
          (m.copy(stack = tail), head)
        }
        case GenerateRandom => State { m =>
          val randomValue = new Random(m.randomSeed).nextInt()
          (m.copy(randomSeed = m.randomSeed + 1), randomValue)
        }
        case DrawSprite(x, y, bytes) => State { m =>
          val (newDisplay, collision) = DisplayUtils.drawSprite(x, y, bytes, 64, 32, m.display)
          (m.copy(display = newDisplay), collision)
        }
        case GetKeyDown(key) => State.inspect(_.inputState(key))
        case ReadByte(address) => State.inspect(_.memory.get(address))
        case WriteByte(address, value) => State.modify[Machine](m => m.copy(memory = m.memory.splice(address, ByteVector(value))))
        case ReadRegister(register) => State.inspect(_.registers(register))
        case WriteRegister(register, byte) => State.modify[Machine](m => m.copy(registers = m.registers.updated(register, byte)))
        case ReadPc => State.inspect(_.pc)
        case WritePc(address) => State.modify[Machine](_.copy(pc = address))
        case ReadPowerState => State.inspect(_.powerState)
      }
    }
  }
}

