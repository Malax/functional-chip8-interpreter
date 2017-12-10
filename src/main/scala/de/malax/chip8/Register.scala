package de.malax.chip8

sealed trait Register
case object V0 extends Register
case object V1 extends Register
case object V2 extends Register
case object V3 extends Register
case object V4 extends Register
case object V5 extends Register
case object V6 extends Register
case object V7 extends Register
case object V8 extends Register
case object V9 extends Register
case object VA extends Register
case object VB extends Register
case object VC extends Register
case object VD extends Register
case object VE extends Register
case object VF extends Register
case object I extends Register
case object DT extends Register
case object ST extends Register

object Register {
  private val indexed = Vector(
    V0, V1, V2, V3, V4, V5, V6, V7, V8, V9, VA, VB, VC, VD, VE, VF
  )

  def fromIndex(index: Int): Option[Register] = indexed.lift(index)
  val all: Vector[Register] = indexed ++ Vector(I, DT, ST)
  val generalPurposeRegisters: Vector[Register] = indexed
}
