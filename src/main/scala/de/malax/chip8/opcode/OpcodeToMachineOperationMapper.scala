package de.malax.chip8.opcode

import cats.Monad
import cats.implicits._
import de.malax.chip8._
import de.malax.chip8.operations.MachineOperation
import scodec.bits.ByteVector
import de.malax.chip8.operations.MachineOperations.{readRegister, _}
import de.malax.chip8.utils.Bcd

object OpcodeToMachineOperationMapper {
  implicit def bool2int(b: Boolean) = if (b) 1 else 0

  def map(opcode: Opcode): MachineOperation[Unit] = {
    val m = Monad[MachineOperation]

    opcode match {
      case ClearScreen =>
        withIncrementedPc(clearScreen)

      case Return =>
        popStack.flatMap(writePc)

      case JumpToAddress(address) =>
        writePc(address)

      case CallSubroutine(address) =>
        readPc.map(_ + 2).flatMap(pushStack).flatMap(_ => writePc(address))

      case SkipIfEqual(register, byte) => readRegister(register) flatMap { value =>
        incrementProgramCounter(if (value == byte) 4 else 2)
      }

      case SkipIfNotEqual(register, byte) => readRegister(register) flatMap { value =>
        incrementProgramCounter(if (value != byte) 4 else 2)
      }

      case SkipIfEqualRegister(rA, rB) => readRegisters(rA, rB) { (a, b) =>
        incrementProgramCounter(if (a == b) 4 else 2)
      }

      case LoadConstant(r, byte) =>
        withIncrementedPc(writeRegister(r, byte))

      case AddConstant(r, byte) =>
        withIncrementedPc(readRegister(r).map(_ + byte % 0xFF).flatMap(writeRegister(r, _)))

      case LoadRegister(rA, rB) =>
        withIncrementedPc(readRegister(rA).flatMap(writeRegister(rB, _)))

      case OrRegister(rA, rB) =>
        withIncrementedPc(readRegisters(rA, rB) { (a, b) =>
          writeRegister(rA, a | b)
        })

      case AndRegister(rA, rB) =>
        withIncrementedPc(readRegisters(rA, rB) { (a, b) =>
          writeRegister(rA, a & b)
        })

      case XorRegister(rA, rB) =>
        withIncrementedPc(readRegisters(rA, rB) { (a, b) =>
          writeRegister(rA, a ^ b)
        })

      case AddRegister(rA, rB) =>
        withIncrementedPc(readRegisters(rA, rB) { (a, b) =>
          for {
            _ <- writeRegister(rA, (a + b) & 0xFF)
            _ <- writeRegister(VF, (a + b) > 0xFF)
          } yield ()
        })

      case SubtractRegister(rA, rB) =>
        withIncrementedPc(readRegisters(rA, rB) { (a, b) =>
          val borrow = b > a
          for {
            _ <- writeRegister(rA, a - b & 0xFF)
            _ <- writeRegister(VF, !borrow)
          } yield ()
        })

      case ShiftLeft(rA, rB) =>
        withIncrementedPc(
          for {
            vB <- readRegister(rB)
            _ <- writeRegister(VF, if ((vB & 0x80) == 0x80) 1 else 0)
            _ <- writeRegister(rA, (vB << 1) & 0xFF)
          } yield ()
        )

      case ShiftRight(rA, rB) =>
        withIncrementedPc(
          for {
            vB <- readRegister(rB)
            _ <- writeRegister(VF, vB & 0x1)
            _ <- writeRegister(rA, vB >> 1)
          } yield ()
        )

      case SubN(rA, rB) =>
        withIncrementedPc(readRegisters(rA, rB) { (a, b) =>
          for {
            _ <- writeRegister(VF, if (a > b) 1 else 0)
            _ <- writeRegister(rA, (b - a) & 0xFF)
          } yield ()
        })

      case SkipIfNotEqualRegister(rA, rB) =>
        readRegisters(rA, rB) { (a, b) =>
          incrementProgramCounter(if (a == b) 2 else 4)
        }

      case LoadIRegister(byte) =>
        withIncrementedPc(writeRegister(I, byte))

      case JumpToAddressOffsettedByVa(address) =>
        readRegister(VA).map(_ + address).flatMap(incrementProgramCounter)

      case GenerateRandom(r, mask) =>
        withIncrementedPc(generateRandom.map(_ & mask).flatMap(writeRegister(r, _)))

      case DrawSprite(rA, rB, sizeInBytes) =>
        withIncrementedPc(
          for {
            a <- readRegister(rA)
            b <- readRegister(rB)
            i <- readRegister(I)
            byteList <- m.sequence(0.until(sizeInBytes).toList map { offset =>
              readByte(i + offset)
            })
            collision <- drawSprite(a, b, ByteVector(byteList: _*))
            _ <- writeRegister(VF, if (collision) 1 else 0)
          } yield ()
        )

      case SkipIfKeyPressed(r) =>
        for {
          key <- readRegister(r).map(Key.fromIndex(_).get)
          isKeyDown <- getKeyDown(key)
          _ <- incrementProgramCounter(if (isKeyDown) 4 else 2)
        } yield()

      case SkipIfKeyNotPressed(r) =>
        for {
          key <- readRegister(r).map(Key.fromIndex(_).get)
          isKeyDown <- getKeyDown(key)
          _ <- incrementProgramCounter(if (!isKeyDown) 4 else 2)
        } yield()

      case ReadTimerValue(r) =>
        withIncrementedPc(readRegister(DT).flatMap(writeRegister(r, _)))

      case LoadPressedKey(r) =>
        val pressedKey = Key.all.foldLeft(m.pure[Option[Key]](None)) { (acc, key) =>
          acc.map(_.isDefined).ifM(
            acc,
            getKeyDown(key).ifM(m.pure(Some(key)), m.pure(None))
          )
        }

        pressedKey.map(_.isDefined).ifM(
          withIncrementedPc(writeRegister(r, 0)),
          m.unit
        )

      case LoadTimer(r) =>
        withIncrementedPc(readRegister(r).flatMap(writeRegister(DT, _)))

      case LoadSoundTimer(r) =>
        withIncrementedPc(readRegister(r).flatMap(writeRegister(ST, _)))

      case AddRegisterValueToI(r) =>
        withIncrementedPc(readRegisters(r, I) { (a, b) =>
          writeRegister(I, a + b)
        })

      case LoadSpriteAddressForDigit(r) =>
        withIncrementedPc(readRegister(r).map(_ * 5).flatMap(writeRegister(I, _)))

      case BinaryCodedDecimal(r) =>
        withIncrementedPc(readRegisters(r, I) { (a, b) =>
          val bytes = Bcd.encode(a)

          for {
            _ <- writeByte(b, bytes._1)
            _ <- writeByte(b + 1, bytes._2)
            _ <- writeByte(b + 2, bytes._3)
          } yield ()
        })

      case WriteRegistersToMemory(r) =>
        val registers = Register.generalPurposeRegisters.takeWhile(_ != r) :+ r
        withIncrementedPc(
          for {
            address <- readRegister(I)
            b <- m.sequence(registers.map(readRegister)).map(ByteVector(_: _*))
            _ <- patchMemory(address, b)
            _ <- writeRegister(I, address + registers.length + 1)
          } yield ()
        )

      case ReadRegistersFromMemory(r) =>
        val registers = Register.generalPurposeRegisters.takeWhile(_ != r) :+ r

        withIncrementedPc(
          for {
            address <- readRegister(I)
            _ <- m.sequence(registers.zipWithIndex map { kv =>
              val (r, index) = kv
              readByte(address + index).flatMap(writeRegister(r, _))
            })
            _ <- writeRegister(I, address + registers.length + 1)
          } yield ()
        )
    }
  }

  private def withIncrementedPc[A](machineOperation: MachineOperation[A]): MachineOperation[A] = {
    for {
      result <- machineOperation
      pc <- readPc
      _ <- writePc(pc + 2)
    } yield result
  }

  private def readRegisters[A](a: Register, b: Register)(f: (Int, Int) => MachineOperation[A]): MachineOperation[A] = {
    for {
      aValue <- readRegister(a)
      bValue <- readRegister(b)
      result <- f(aValue, bValue)
    } yield result
  }
}
