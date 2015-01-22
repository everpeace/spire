package spire.algebra

package object galois {

  /**
   * GF(2<sup>8</sup>) represented by Byte
   */
  case class GaloisFieldByte(override val primitive_polynomial: Byte)
    extends AbstractGaloisField[Byte](8, primitive_polynomial) {
    import AbstractGaloisField.BIT
    def zero = 0.toByte
    def one = 1.toByte
    def maxValInBigInt = Byte.MaxValue.toLong
    def xor = (a:Byte, b:Byte) => (a ^ b).toByte
    def leftShiftOneBit = (a:Byte) => (a << 1).toByte
    def bit = (a:Byte, i:Int) => a.toLong & BIT(i)
  }

  /**
   * GF(2<sup>16</sup>) represented by Short
   */
  case class GaloisFieldShort(override val primitive_polynomial: Short)
    extends AbstractGaloisField[Short](16, primitive_polynomial) {
    import AbstractGaloisField.BIT
    def zero = 0.toShort
    def one = 1.toShort
    def maxValInBigInt = Short.MaxValue.toLong
    def xor = (a:Short,b:Short) => (a ^ b).toShort
    def leftShiftOneBit = (a:Short) => (a << 1).toShort
    def bit = (a:Short,i:Int) => a.toLong & BIT(i)
  }

  /**
   * GF(2<sup>32</sup>) represented by Int
   */
  case class GaloisFieldInt(override val primitive_polynomial: Int)
    extends AbstractGaloisField[Int](32, primitive_polynomial) {
    import AbstractGaloisField.BIT
    def zero = 0
    def one = 1
    def maxValInBigInt = Int.MaxValue.toLong
    def xor = (a: Int, b: Int) => a ^ b
    def leftShiftOneBit = (a: Int) => a << 1
    def bit = (a: Int, i: Int) => a.toLong & BIT(i)
  }

  /**
   * GF(2<sup>64</sup>) represented by Long
   */
  case class GaloisFieldLong(override val primitive_polynomial: Long)
    extends AbstractGaloisField[Long](64, primitive_polynomial) {
    import AbstractGaloisField.BIT
    def zero = 0L
    def one = 1L
    def maxValInBigInt = Long.MaxValue
    def xor = (a:Long, b:Long)=> a^b
    def leftShiftOneBit = (a:Long) => a << 1
    def bit = (a:Long, i:Int) => a & BIT(i)
  }

  /**
   * GF(2<sup>d</sup>) represented by Array[Byte]
   */
  case class GaloisFieldArrayByte(override val degree: Int, override val primitive_polynomial: Array[Byte])
    extends AbstractGaloisField[Array[Byte]](degree, primitive_polynomial) {
    import AbstractGaloisField.BIT

    require(degree % 8 == 0 && degree >= 8,
      "degree must be power of 2 and no less than 8. degree=" + degree)

    def zero = Array.fill(len)(0.toByte)

    def one = {
      val z = zero
      z(z.length-1) = 1.toByte
      z
    }

    // 2^(degree-1)-1
    def maxValInBigInt = {
      val a = new Array[Byte](len)
      (len-1 to 1 by -1).foreach(a(_) = (0x00ff).toByte)
      a(0) = 0x007f.toByte
      BigInt(a)
    }

    def xor = (a: Array[Byte], b: Array[Byte])
    => a.zip(b).map(t => ((t._1 ^ t._2)&0x00ff).toByte)

    def leftShiftOneBit = (a: Array[Byte]) =>
      a.zipWithIndex.foldRight((zero, 0.toByte)){
        (a_i: (Byte, Int), b_c: (Array[Byte], Byte)) => {
          val input_carry = b_c._2
          //assign shifted value
          b_c._1(a_i._2) = ((a_i._1 << 1).toByte | input_carry).toByte
          val output_carry = ((a_i._1 >> 7) & 0x0001).toByte
          (b_c._1, output_carry)
        }
      }._1

    def bit = (a: Array[Byte], i: Int) =>
      a(a.length - 1 - (i / 8)).toLong & BIT(i % 8)

    private[this] def len = degree / 8
  }
}
