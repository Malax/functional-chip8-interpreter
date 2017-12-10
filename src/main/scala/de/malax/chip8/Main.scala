package de.malax.chip8

import java.io.{File, FileInputStream}

import cats._
import de.malax.chip8.compilers.swing.SwingGuiCompiler
import de.malax.chip8.operations.MachineOperation
import de.malax.chip8.opcode.{OpcodeParser, OpcodeToMachineOperationMapper}
import org.apache.commons.io.IOUtils
import scodec.bits.ByteVector
import de.malax.chip8.operations.MachineOperations._

object Main extends App {
  val rom = args.headOption map { romFilePath =>
    println(romFilePath)
    ByteVector.apply(IOUtils.toByteArray(new FileInputStream(new File(romFilePath))))
  } getOrElse {
    println("Please give a path to a Chip-8 ROM as the first argument!")
    sys.exit(-1)
  }

  val machineOperationMonad = Monad[MachineOperation]

  val cycle = for {
    pc <- readPc
    opcodeByte <- machineOperationMonad.map2(readByte(pc), readByte(pc + 1))((hi, lo) => hi << 8 | lo)
    _ <- OpcodeParser.parse(opcodeByte).map(OpcodeToMachineOperationMapper.map).getOrElse(machineOperationMonad.unit)
    powerState <- readPowerState
  } yield powerState

  val program = for {
    _ <- patchMemory(0x200, rom)
    _ <- machineOperationMonad.iterateUntil(cycle)(powerState => !powerState)
  } yield ()

  program.foldMap(SwingGuiCompiler.compiler).unsafeRunSync()

  println("Done!")
}
