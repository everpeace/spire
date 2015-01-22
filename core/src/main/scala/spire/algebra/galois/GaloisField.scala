package spire.algebra.galois

import spire.algebra.Field

/**
 * GF(2 ^ degee) : Galois Extension Field based on GF(2)
 * http://mathworld.wolfram.com/FiniteField.html
 *
 * @tparam E
 */
trait GaloisField[E] extends Field[E] {
  require(degree > 0, "degree of GaloisField must be positive integer.")

  def degree:Int

  /**
   * primitive polynomial represented by E.
   * The highest degree term is omitted in this representation
   * because coefficient of the highest degree must be one.
   * e.g. x^32 + x^22 + x^2 + x + 1 is represented as
   *      0000 0100 0000 0000 0000 0000 0111 in binary representation.
   *   0x    0    4    0    0    0    0    7 in hex representation.
   */
  def primitive_polynomial: E

  /**
   * @return polynomial representation of e, like "x^3+x^2+1".
   */
  def polynomialRepresentation(e: E): String

  /**
   * @return primitive polynomial representation of this galois field.
   */
  def primitivePolynomialString
  = if (degree == 1) "x+" + polynomialRepresentation(primitive_polynomial)
    else             "x^" + degree + "+" + polynomialRepresentation(primitive_polynomial)
}


object GaloisField {
  @inline final def apply[E](implicit gf: GaloisField[E]): GaloisField[E] = gf
}
