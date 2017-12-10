package de.malax.chip8

import cats.free.Free

package object operations {
  type MachineOperation[A] = Free[MachineOperationA, A]
}
