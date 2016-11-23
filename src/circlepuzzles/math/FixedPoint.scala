package circlepuzzles.math

import java.math.{BigDecimal, RoundingMode}

/**
  * Immutable, fixed-point signed decimal numbers. The number of of decimal places stored during computation, as well
  * as the number of decimal decimal places used for comparison, are found in the `FixedPoint` companion object.
  *
  * This is to be used in place of Scala's [[scala.math.BigDecimal]] wrapper where fixed precision is needed.
  * @param bd Arbitrary precision `BigDecimal` that this instance approximates, using the maximum number of allowed
  * places after the decimal point. The scale will be set to `FixedPoint.computeScale`, rounding using
  * `FixedPoint.roundingMode`.
  */
class FixedPoint(bd: BigDecimal) extends Ordered[FixedPoint] {
  import FixedPoint._

  /**
    * Computes the sum `this + that`.
    */
  def +(that: FixedPoint): FixedPoint = new FixedPoint(value.add(that.value))

  /**
    * Computes the difference `this - that`.
    */
  def -(that: FixedPoint): FixedPoint = new FixedPoint(value.subtract(that.value))

  /**
    * Computes the product `this * that`.
    */
  def *(that: FixedPoint): FixedPoint = new FixedPoint(value.multiply(that.value))

  /**
    * Computes the quotient `this / that`.
    */
  def /(that: FixedPoint): FixedPoint = new FixedPoint(value.divide(that.value, computeScale, roundingMode))

  /**
    * Computes the absolute value `|this|`
    */
  def abs: FixedPoint = new FixedPoint(value.abs())

  /**
    * Computes the exponent `this ** n`
    */
  def pow(n: Int): FixedPoint = new FixedPoint(value.pow(n))

  /**
    * Computes the negation `-this`.
    */
  def unary_- : FixedPoint = new FixedPoint(value.negate())

  // TODO: might want implementations of %, /%, max, min

  /**
    * Underlying `BigDecimal` representation.
    */
  val value = bd.setScale(computeScale, roundingMode)

  /**
    * Compare this and another `FixedPoint` by computing the difference `this - that` and rounding this difference
    * to `FixedPoint.compareScale` places after the decimal point.
    * @param that `FixedPoint` to compare to.
    * @return -1, 0, or 1 if the difference is respectively negative, zero, or positive.
    */
  override def compare(that: FixedPoint): Int = {
    val difference = value.subtract(that.value)
    difference.setScale(compareScale, roundingMode).signum()
  }

  /**
    * Tests if the other object is an equal `FixedPoint`.
    * @param that Object to compare to.
    * @return True if and only if `that` is a `FixedPoint` instance, and the difference `this - that` is equal to zero
    * when rounded to `FixedPoint.compareScale` places after the decimal point.
    */
  override def equals(that: Any): Boolean = {
    that match {
      case thatFP: FixedPoint => compare(thatFP) == 0
      case _ => false
    }
  }
}

/**
  * Utilities and constants associated with `FixedPoint` arithmetic.
  */
object FixedPoint {
  /**
    * Number of decimal places after the decimal point stored for `FixedPoint` instances.
    */
  val computeScale = 40

  /**
    * Number of decimal places after the decimal points used to compare `FixedPoint` instances.
    */
  val compareScale = 20

  /**
    * Rounding mode used for intermediate computation.
    */
  val roundingMode = RoundingMode.HALF_EVEN
}
