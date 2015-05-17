package me.axiometry.agario.net

case class DataView(array: Array[Byte], offset: Int, length: Int)(implicit val byteOrder: ByteOrder = ByteOrder.BigEndian) {
  import ByteOrder._

  def getInt8(off: Int)(implicit byteOrder: ByteOrder): Byte =
    array(off)
  def setInt8(off: Int, value: Byte)(implicit byteOrder: ByteOrder): Unit =
    array(off) = value

  def getUint8(off: Int)(implicit byteOrder: ByteOrder): Int =
    array(off) & 0xFF
  def setUint8(off: Int, value: Int)(implicit byteOrder: ByteOrder): Unit =
    array(off) = value.toByte

  def getInt16(off: Int)(implicit byteOrder: ByteOrder): Short = byteOrder match {
    case BigEndian =>
      ((array(off).toInt << 8)
          + array(off+1).toInt).toShort
    case LittleEndian =>
      ((array(off).toInt << 8)
          + array(off).toInt).toShort
  }
  def setInt16(off: Int, value: Short)(implicit byteOrder: ByteOrder): Unit = byteOrder match {
    case BigEndian =>
      array(off) = ((value >>> 8) & 0xFF).toByte
      array(off+1) = (value & 0xFF).toByte
    case LittleEndian =>
      array(off+1) = ((value >>> 8) & 0xFF).toByte
      array(off) = (value & 0xFF).toByte
  }

  def getUint16(off: Int)(implicit byteOrder: ByteOrder): Int = byteOrder match {
    case BigEndian =>
      ((array(off).toInt << 8)
          + array(off+1))
    case LittleEndian =>
      ((array(off+1).toInt << 8)
          + array(off))
  }
  def setUint16(off: Int, value: Int)(implicit byteOrder: ByteOrder): Unit = byteOrder match {
    case BigEndian =>
      array(off) = ((value >>> 8) & 0xFF).toByte
      array(off+1) = (value & 0xFF).toByte
    case LittleEndian =>
      array(off+1) = ((value >>> 8) & 0xFF).toByte
      array(off) = (value & 0xFF).toByte
  }

  def getInt32(off: Int)(implicit byteOrder: ByteOrder): Int = byteOrder match {
    case BigEndian =>
      ((array(off).toInt << 24)
          + (array(off+1).toInt << 16)
          + (array(off+2).toInt << 8)
          + array(off+3).toInt)
    case LittleEndian =>
      ((array(off+3).toInt << 24)
          + (array(off+2).toInt << 16)
          + (array(off+1).toInt << 8)
          + array(off).toInt)
  }
  def setInt32(off: Int, value: Int)(implicit byteOrder: ByteOrder): Unit = byteOrder match {
    case BigEndian =>
      array(off) = ((value >>> 24) & 0xFF).toByte
      array(off+1) = ((value >>> 16) & 0xFF).toByte
      array(off+2) = ((value >>> 8) & 0xFF).toByte
      array(off+3) = (value & 0xFF).toByte
    case LittleEndian =>
      array(off+3) = ((value >>> 24) & 0xFF).toByte
      array(off+2) = ((value >>> 16) & 0xFF).toByte
      array(off+1) = ((value >>> 8) & 0xFF).toByte
      array(off) = (value & 0xFF).toByte
  }

  def getUint32(off: Int)(implicit byteOrder: ByteOrder): Long = byteOrder match {
    case BigEndian =>
      ((array(off).toLong << 24)
          + (array(off+1).toLong << 16)
          + (array(off+2).toLong << 8)
          + array(off+3).toLong)
    case LittleEndian =>
      ((array(off+3).toLong << 24)
          + (array(off+2).toLong << 16)
          + (array(off+1).toLong << 8)
          + array(off).toLong)
  }
  def setUint32(off: Int, value: Long)(implicit byteOrder: ByteOrder): Unit = byteOrder match {
    case BigEndian =>
      array(off) = ((value >>> 24) & 0xFF).toByte
      array(off+1) = ((value >>> 16) & 0xFF).toByte
      array(off+2) = ((value >>> 8) & 0xFF).toByte
      array(off+3) = (value & 0xFF).toByte
    case LittleEndian =>
      array(off+3) = ((value >>> 24) & 0xFF).toByte
      array(off+2) = ((value >>> 16) & 0xFF).toByte
      array(off+1) = ((value >>> 8) & 0xFF).toByte
      array(off) = (value & 0xFF).toByte
  }

  def getFloat32(off: Int)(implicit byteOrder: ByteOrder): Float =
    java.lang.Float.intBitsToFloat(getInt32(off))
  def setFloat32(off: Int, value: Float)(implicit byteOrder: ByteOrder): Unit =
    setInt32(off, java.lang.Float.floatToRawIntBits(value))

  def getFloat64(off: Int)(implicit byteOrder: ByteOrder): Double =
    java.lang.Double.longBitsToDouble(getUint32(off))
  def setFloat64(off: Int, value: Double)(implicit byteOrder: ByteOrder): Unit =
    setUint32(off, java.lang.Double.doubleToRawLongBits(value))
}
object DataView {
  def apply(length: Int): DataView = DataView(Array.ofDim(length), 0, length)
}

sealed trait ByteOrder
object ByteOrder {
  case object LittleEndian extends ByteOrder
  case object BigEndian extends ByteOrder
}