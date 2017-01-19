package circlepuzzles.math

import java.math.BigDecimal

import scala.util.Random

/**
  * Immutable, fixed-point signed decimal numbers. The number of of decimal places stored during computation, as well
  * as the number of decimal decimal places used for comparison, are found in the `FixedPoint` companion object. This
  * means that `FixedPoint` instances are compared using fewer digits than are used for computation.
  *
  * This is to be used in place of Scala's [[scala.math.BigDecimal]] wrapper where fixed precision is needed.
  * @param bd Arbitrary precision `BigDecimal` that this instance will approximate using `FixedPoint.ComputeScale`
  * digits after the decimal point. If rounding is necessary, this will use `FixedPoint.RoundingMode`.
  */
class FixedPoint(bd: BigDecimal) extends Ordered[FixedPoint] {
  import FixedPoint._

  /**
    * Underlying `BigDecimal` representation, which necessarily has scale `FixedPoint.ComputeScale`.
    */
  val value = bd.setScale(ComputeScale, RoundingMode)

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
  def /(that: FixedPoint): FixedPoint = new FixedPoint(value.divide(that.value, ComputeScale, RoundingMode))

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

  /**
    * Computes the signum function, i.e. -1, 0, or 1 if this is respectively negative, 0, or positive.
    */
  def signum: Int = compare(FixedPoint.Zero)

  /**
    * Computes a rounded version of this `FixedPoint` that should be used for comparison with other `FixedPoint`
    * instances. This works by adding a fixed random offset, then rounding to `FixedPoint.CompareScale` places after the
    * decimal point.
    * @return A `BigDecimal` that can be used to compare `FixedPoint` instances.
    */
  def compareValue: BigDecimal = {
    val unrounded = value.add(Offset)
    unrounded.setScale(CompareScale, RoundingMode)
  }

  /**
    * Compare this and another `FixedPoint` by comparing the corresponding `compareValue`s.
    * @param that `FixedPoint` to compare to.
    * @return -1, 0, or 1 if `this.compareValue` is respectively less than, equal to, or greater than
    * `that.compareValue`.
    */
  override def compare(that: FixedPoint): Int = {
    compareValue.compareTo(that.compareValue)
  }

  /**
    * Tests if the other object is an equal `FixedPoint`.
    * @param that Object to compare to.
    * @return `true` if and only if `that` is a `FixedPoint` instance that is equal according to `compare`.
    */
  override def equals(that: Any): Boolean = {
    that match {
      case thatFP: FixedPoint => compare(thatFP) == 0
      case _ => false
    }
  }

  /**
    * Returns a hash code that satisfies the [[Object]] contract, which is to say that the hash depends only on
    * `compareValue`.
    * @return A hash code depending only on `compareValue`.
    */
  override def hashCode: Int = compareValue.hashCode()

  /**
    * Returns the unrounded string representation of this `FixedPoint` instance.
    * @return A string representation of this `FixedPoint`.
    */
  override def toString: String = value.toString

  /**
    * Returns the `Double` closest in value of this `FixedPoint`.
    * @return A `Double` approximately equal to this.
    */
  def toDouble: Double = value.doubleValue()
}

/**
  * Utilities and constants associated with `FixedPoint` arithmetic.
  */
object FixedPoint {
  /**
    * Converts an integer to its corresponding `FixedPoint` value.
    * @param value An integer.
    * @return `FixedPoint` with value represented by the integer.
    */
  def apply(value: Int): FixedPoint = new FixedPoint(new BigDecimal(value))

  /**
    * Converts a string to its corresponding `FixedPoint` value.
    * @param value A decimal string.
    * @return `FixedPoint` with value represented by the string.
    */
  def apply(value: String): FixedPoint = new FixedPoint(new BigDecimal(value))

  /**
    * Number of decimal places after the decimal point stored for `FixedPoint` instances.
    */
  val ComputeScale = 40

  /**
    * Number of decimal places after the decimal points used to compare `FixedPoint` instances.
    */
  val CompareScale = 20

  /**
    * Rounding mode used for intermediate computation and comparisons.
    */
  val RoundingMode = BigDecimalMath.RoundingMode

  /**
    * Randomly generated offset that is added to `FixedPoint` instances before rounding for comparison. The use of
    * this random offset guarantees that the probability that a given `FixedPoint` lies close to a rounding cutoff is
    * small. In particular, this means that if a few digits of precision are lost, a `FixedPoint` will still almost
    * always hash into the same bucket.
    *
    * The correctness of code involving `FixedPoint` comparisons will often rely on the assumption that this probability
    * is effectively 0. This is a reasonable assumption if the sum of the number of digits of lost precision and the
    * order of magnitude of the number of `FixedPoint` operations performed is much smaller than the difference
    * `ComputeScale - CompareScale`. If this assumption ever turns out to be false, the state of the program may become
    * corrupted.
    */
  val Offset = {
    val zeros = "0" * CompareScale
    val digits = List.fill(ComputeScale - CompareScale)(Random.nextInt(10))
    new BigDecimal("0." + zeros + digits.mkString)
  }

  /**
    * `FixedPoint` with value closest to pi/2.
    */
  val HalfPi = new FixedPoint(BigDecimalMath.halfPi(ComputeScale))

  /**
    * `FixedPoint` with value closest to pi.
    */
  val Pi = new FixedPoint(BigDecimalMath.pi(ComputeScale))

  /**
    * `FixedPoint` with value closest to 3*pi/2.
    */
  val ThreeHalvesPi = new FixedPoint(BigDecimalMath.threeHalvesPi(ComputeScale))

  /**
    * `FixedPoint` with value closest to 2*pi.
    */
  val TwoPi = new FixedPoint(BigDecimalMath.twoPi(ComputeScale))

  /**
    * `FixedPoint` with value 0.
    */
  val Zero = new FixedPoint(BigDecimal.ZERO)

  /**
    * `FixedPoint` with value 1.
    */
  val One = new FixedPoint(BigDecimal.ONE)

  /**
    * `FixedPoint` with value 2.
    */
  val Two = new FixedPoint(BigDecimalMath.Two)

  /**
    * Computes the angle theta whose tangent equals the ratio `y / x` and is in the same quadrant as `(y, x)`. Note that
    * this method differs from the standard definition of `atan2` methods in that it computes the result to be in the
    * range [0,2*pi).
    * @param y Y-coordinate.
    * @param x X-coordinate.
    * @return The two argument arc tangent of `y` and `x` in the range [0,2*pi).
    * @throws ArithmeticException If `y` and `x` are both zero.
    */
  def atan2Mod2Pi(y: FixedPoint, x: FixedPoint): FixedPoint = {
    mod2Pi(new FixedPoint(BigDecimalMath.atan2(y.value, x.value, ComputeScale)))
  }

  /**
    * Normalizes the angle to be in the range [0,2*pi).
    * @param theta Angle to normalize, in radians.
    * @return Equivalent angle in the range [0,2*pi).
    */
  def mod2Pi(theta: FixedPoint): FixedPoint = {
    // We can't use BigDecimalMath here because a FixedPoint could compare as equal to TwoPi despite having an
    // underlying representation that is strictly less than 2*pi
    var result = theta
    while(result < Zero) result += TwoPi
    while(result >= TwoPi) result -= TwoPi
    result
  }

  /**
    * Computes the square root of the argument. Returns 0 if x is negative.
    * @param x Argument whose square root is to be computed.
    * @return Nonnegative square root of the argument, or 0 if it is negative.
    */
  def sqrt(x: FixedPoint): FixedPoint = {
    new FixedPoint(BigDecimalMath.sqrt(x.value, ComputeScale))
  }

  /**
    * Computes the sine of the argument.
    * @param theta Angle whose sine is to be computed, in radians.
    * @return Sine of the argument.
    */
  def sin(theta: FixedPoint): FixedPoint = {
    new FixedPoint(BigDecimalMath.sin(theta.value, ComputeScale))
  }

  /**
    * Computes the cosine of the argument.
    * @param theta Angle whose cosine is to be computed, in radians.
    * @return Cosine of the argument.
    */
  def cos(theta: FixedPoint): FixedPoint = {
    new FixedPoint(BigDecimalMath.cos(theta.value, ComputeScale))
  }

  /**
    * Computes the arc sine of the argument. Returns pi/2 if the argument is greater than 1, and -pi/2 if it is less
    * than -1.
    * @param x Argument whose arc sine is to be computed.
    * @return Arc sine of the argument in the range [-pi/2,pi/2].
    */
  def asin(x: FixedPoint): FixedPoint = {
    new FixedPoint(BigDecimalMath.asin(x.value, ComputeScale))
  }

  /**
    * Computes the arc cosine of the argument. Returns 0 if the argument is greater than 1, and pi if it is less
    * than -1.
    * @param x Argument whose arc cosine is to be computed.
    * @return Arc cosine of the argument in the range [0,pi].
    */
  def acos(x: FixedPoint): FixedPoint = {
    new FixedPoint(BigDecimalMath.acos(x.value, ComputeScale))
  }
}
