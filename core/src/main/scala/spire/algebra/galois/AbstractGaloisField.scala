package spire.algebra.galois

/**
 * Abstract class for GaloisField[Byte, Short, Int, Long, Array[Byte]].
 */
abstract class AbstractGaloisField[T](override val degree:Int, override val primitive_polynomial:T)
      extends GaloisField[T] { self:GaloisField[T] =>

  // operations which are specific to T.
  def zero: T
  def one: T
  def maxValInBigInt: BigInt
  def xor: (T, T) => T
  def leftShiftOneBit: T => T
  def bit: (T, Int) => Long


  // additions
  def negate(e: T) = e
  def plus(a: T, b: T): T = xor(a, b) // addition of GF(2^D) is XOR.


  // multiplications
  def times(a: T, b: T): T = (a, b) match {
    case (x, y) if x == zero || b == zero => zero
    case _ => {
      var ret: T = zero;
      for (i <- 0 until degree) {
        // if b includes x^i (x is primitive_polynomial of primitive polynomial.)
        if (bit(b, i) != 0L) {
          // add a * x^i
          ret = plus(ret, mul_rootpow(a, i));
        }
      }
      ret
    }
  }
  override def pow(a: T, p: Int) = _pow(a, p)
  def _pow(a: T, p: BigInt): T = p match{
    case _ if (p < 0) => _pow(reciprocal(a), -1*p)
    case _ if (p == 0) => one
    case _ if ((p % 2) == 0) => _pow(times(a,a), p/2)
    case _ if ((p % 2) == 1) => times(a, _pow(a, p-1))
  }

  // divisions
  def div(x: T, y: T): T = times(x, reciprocal(y))
  def quot(x: T, y: T) = div(x, y)
  def mod(a: T, b: T): T = zero
  override def reciprocal(e: T) = {
    // FIXME: this is too naive: e^(-1) = e^(2^l-2) for any e.
    // let l be bit length of T, then T can't represent 2^l-1 because it is signed,
    // so we can compute inverse by e^((2^l-1)*2).
    require(e != zero, "inverse of zero doesn't exist.")
    _pow(_pow(e, maxValInBigInt), 2)
  }

  // TODO
  def gcd(a: T, b: T): T = ???

  
  def polynomialRepresentation(e: T) = {
    val ret: StringBuffer = new StringBuffer()
    var isFirst = true
    for (i<- (degree-1) to 0 by -1 ){
      // add x^i if i-th left bit was 1.
      if ((bit(e, i)) != 0L) {
        if (isFirst == false) {
          ret.append("+")
        } else {
          isFirst = false
        }
        // add term with the degree of i
        i match {
          case 0 =>
            ret.append("1");
          case 1 =>
            ret.append("x");
          case _ => {
            ret.append("x^");
            ret.append(i);
          }
        }
      }
    }
    val retStr = ret.toString()
    if(retStr =="") "0" else retStr;
  }


  // Privates
  /**
   * @return x * e for e /in GF(2^D),
   *         where x be a root of (its primitive polynomial = 0).
   */
  private[this] def mul_root(e: T): T =
    if ((bit(e,degree - 1)) != 0L) {
      // if e has x^D-1, we have to add carry x^D.
      // e.g. assume primitive polynomial be x^32 + x^22 + x^2 + x + 1 (D=32)
      //      x^32 = x^22 + x^2 + x + 1 = primitive_polynomial
      plus(leftShiftOneBit(e), primitive_polynomial)
    } else {
      // otherwise, just left shift.
      leftShiftOneBit(e)
    }

  /**
   * @return x^i * e  for e /in GF(2^D),
   *         where x be a root of (its primitive polynomial = 0).
   */
  private[this] def mul_rootpow(a: T, i: Int): T = i match {
    case x if x >= degree || x < 0 => throw new UnsupportedOperationException("i must be less than " + degree + "[" + i + "]");
    case 0L => a
    case _ => {
      var ret: T = a;
      for (j <- 1 to i) {
        ret = mul_root(ret);
      }
      ret;
    }
  }
}

object AbstractGaloisField{
  /**
   * bit mask for Long
   */
  val BIT: Array[Long] = Array(
    1L <<  0, 1L <<  1, 1L <<  2, 1L <<  3, 1L <<  4, 1L <<  5, 1L <<  6, 1L <<  7,
    1L <<  8, 1L <<  9, 1L << 10, 1L << 11, 1L << 12, 1L << 13, 1L << 14, 1L << 15,
    1L << 16, 1L << 17, 1L << 18, 1L << 19, 1L << 20, 1L << 21, 1L << 22, 1L << 23,
    1L << 24, 1L << 25, 1L << 26, 1L << 27, 1L << 28, 1L << 29, 1L << 30, 1L << 31,
    1L << 32, 1L << 33, 1L << 34, 1L << 35, 1L << 36, 1L << 37, 1L << 38, 1L << 39,
    1L << 40, 1L << 41, 1L << 42, 1L << 43, 1L << 44, 1L << 45, 1L << 46, 1L << 47,
    1L << 48, 1L << 49, 1L << 50, 1L << 51, 1L << 52, 1L << 53, 1L << 54, 1L << 55,
    1L << 56, 1L << 57, 1L << 58, 1L << 59, 1L << 60, 1L << 61, 1L << 62, 1L << 63)
}