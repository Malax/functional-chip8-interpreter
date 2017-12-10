package de.malax.chip8

sealed trait Key
case object Key0 extends Key
case object Key1 extends Key
case object Key2 extends Key
case object Key3 extends Key
case object Key4 extends Key
case object Key5 extends Key
case object Key6 extends Key
case object Key7 extends Key
case object Key8 extends Key
case object Key9 extends Key
case object KeyA extends Key
case object KeyB extends Key
case object KeyC extends Key
case object KeyD extends Key
case object KeyE extends Key
case object KeyF extends Key

object Key {
  private val indexed = Vector[Key](
    Key0, Key1, Key2, Key3, Key4, Key5, Key6, Key7, Key8, Key9, KeyA, KeyB, KeyC, KeyD, KeyE, KeyF
  )

  def fromIndex(index: Int): Option[Key] = indexed.lift(index)
  def toIndex(key: Key): Int = indexed.indexOf(key)
  val all = indexed
}
