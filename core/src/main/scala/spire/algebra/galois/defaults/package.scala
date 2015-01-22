package spire.algebra.galois

package object defaults {

  object primitive_polynomials {
    /** ｘ<sup>8</sup> + ｘ<sup>4</sup> + ｘ<sup>3</sup> + ｘ<sup>2</sup> + 1 */
    val P8: Byte = ((1 << 4) | (1 << 3) | (1 << 2) | 1).toByte

    /** ｘ<sup>16</sup> + ｘ<sup>12</sup> + ｘ<sup>3</sup> + ｘ + 1 */
    val P16: Short = ((1 << 12) | (1 << 3) | (1 << 1) | 1).toShort

    /** x<sup>32</sup> + x<sup>22</sup> + ｘ<sup>2</sup> + ｘ + 1 */
    val P32: Int = ((1 << 22) | (1 << 2) | (1 << 1) | 1)

    /** x<sup>64</sup> + x<sup>4</sup> + x<sup>3</sup> + x + 1 */
    val P64: Long = ((1 << 4) | (1 << 3) | (1 << 1) | (1)).toLong

    /** ｘ<sup>128</sup> + ｘ<sup>8</sup> + ｘ<sup>6</sup> + ｘ<sup>5</sup> + ｘ<sup>4</sup> + ｘ +1 */
    val P128 = (1 << 8) | (1 << 6) | (1 << 5) | (1 << 4) | (1 << 1) | 1
  }

  import primitive_polynomials._
  /** default Galois Field instance on Byte (GF(2<sup>8</sup>), represented in 8 bits) */
  val GF_BYTE = GaloisFieldByte(P8)

  /** default Galois Field instance on Short (GF(2<sup>16</sup>), represented in 16 bits) */
  val GF_SHORT = GaloisFieldShort(P16)

  /** default Galois Field instance on Int (GF(2<sup>32</sup>), represented in 32 bits) */
  val GF_INT = GaloisFieldInt(P32)

  /** default Galois Field instance on Long (GF(2<sup>64</sup>), represented in 64 bits) */
  val GF_LONG = GaloisFieldLong(P64)
}
