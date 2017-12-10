package de.malax.chip8.utils

import de.malax.chip8.{VA, VB, VF}
import de.malax.chip8.compilers.pure.{Machine, PureCompiler}
import de.malax.chip8.opcode._
import de.malax.chip8.operations.MachineOperation
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.BitVector

class WhateverSpec extends FlatSpec with Matchers {

  "PureCompiler" should "execute ClearScreen" in {
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(ClearScreen), Machine.initial)
    m.display should be (BitVector.fill(64 * 32)(high = false))
  }

  it should "execute Return" in {
    val machineState = Machine.initial.copy(stack = List(0xAAA, 0xBBB))
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(Return), machineState)
    m.stack should be (List(0xBBB))
    m.pc should be (0xAAA)
  }

  it should "execute JumpToAddress" in {
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(JumpToAddress(0xABC)), Machine.initial)
    m.pc should be (0xABC)
  }

  it should "execute CallSubroutine" in {
    val machineState = Machine.initial.copy(stack = List(0xAAA, 0xBBB), pc = 0x222)
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(CallSubroutine(0xABC)), machineState)

    m.pc should be (0xABC)
    m.stack.head should be (0x224)
  }

  it should "execute successful SkipIfEqual" in {
    val machineState = Machine.initial.copy(stack = List(0xAAA, 0xBBB), pc = 0x222).withRegisterValue(VB, 0xFF)
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(SkipIfEqual(VB, 0xFF)), machineState)

    m should be (machineState.copy(pc = 0x226))
  }

  it should "execute unsuccessful SkipIfEqual" in {
    val machineState = Machine.initial.copy(stack = List(0xAAA, 0xBBB), pc = 0x222).withRegisterValue(VB, 0xF0)
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(SkipIfEqual(VB, 0xFF)), machineState)

    m should be (machineState.copy(pc = 0x224))
  }

  it should "execute AddRegister with overflow correctly" in {
    val machine = Machine.initial.withRegisterValue(VA, 0xAA).withRegisterValue(VB, 0xAA)
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(AddRegister(VA, VB)), machine)
    m should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0x54).withRegisterValue(VF, 0x01))
  }

  it should "execute AddRegister without overflow correctly" in {
    val machine = Machine.initial.withRegisterValue(VA, 0x0A).withRegisterValue(VB, 0x01)
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(AddRegister(VA, VB)), machine)
    m should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0x0B).withRegisterValue(VF, 0x00))
  }

  it should "execute AddConstant with overflow correctly" in {
    val machine = Machine.initial.withRegisterValue(VA, 0x0A)
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(AddConstant(VA, 0xFF)), machine)
    m should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0x0A))
  }

  it should "execute AddConstant without overflow correctly" in {
    val machine = Machine.initial.withRegisterValue(VA, 0x0A)
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(AddConstant(VA, 0x02)), machine)
    m should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0x0C))
  }

  it should "execute AddConstant without borrow correctly" in {
    val machine = Machine.initial.withRegisterValue(VA, 0x0A).withRegisterValue(VB, 0x01)
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(SubtractRegister(VA, VB)), machine)
    m should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0x09).withRegisterValue(VF, 0x01))
  }

  it should "execute ShiftLeft correctly" in {
    val machine = Machine.initial.withRegisterValue(VA, 0x00).withRegisterValue(VB, 0xFF)
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(ShiftLeft(VA, VB)), machine)
    m should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0xFE).withRegisterValue(VF, 0x01))
  }

  it should "execute RightLeft correctly" in {
    val machine = Machine.initial.withRegisterValue(VA, 0x00).withRegisterValue(VB, 0xFF)
    val (m, _) = execute(OpcodeToMachineOperationMapper.map(ShiftRight(VA, VB)), machine)
    m should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0x7F).withRegisterValue(VF, 0x01))
  }

  private def execute[A](p: MachineOperation[A], m: Machine): (Machine, A) = {
    p.foldMap(PureCompiler.compiler).run(m).value
  }
}
