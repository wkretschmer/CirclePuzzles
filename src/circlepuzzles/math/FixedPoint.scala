package circlepuzzles.math

import java.math.{BigDecimal, MathContext, RoundingMode}

import org.nevec.rjm.BigDecimalMath

import scala.util.Random

/**
  * Immutable, fixed-point signed decimal numbers. The number of of decimal places stored during computation, as well
  * as the number of decimal decimal places used for comparison, are found in the `FixedPoint` companion object. This
  * means that `FixedPoint` instances are compared using fewer digits than are used for computation.
  *
  * This is to be used in place of Scala's [[scala.math.BigDecimal]] wrapper where fixed precision is needed.
  * @param bd Arbitrary precision `BigDecimal` that this instance will approximate using `FixedPoint.computeScale`
  * digits after the decimal point. If rounding is necessary, this will use `FixedPoint.roundingMode`.
  */
class FixedPoint(bd: BigDecimal) extends Ordered[FixedPoint] {
  import FixedPoint._

  /**
    * Underlying `BigDecimal` representation, which necessarily has scale `FixedPoint.computeScale`.
    */
  val value = bd.setScale(computeScale, roundingMode)

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

  /**
    * Computes a rounded version of this `FixedPoint` that should be used for comparison with other `FixedPoint`
    * instances. This works by adding a fixed random offset, then rounding to `FixedPoint.compareScale` places after the
    * decimal point.
    * @return A `BigDecimal` that can be used to compare `FixedPoint` instances.
    */
  def compareValue: BigDecimal = {
    val unrounded = value.add(offset)
    unrounded.setScale(compareScale, roundingMode)
  }

  /**
    * Compare this and another `FixedPoint` by comparing the corresponding `compareValue`s.
    * @param that `FixedPoint` to compare to.
    * @return A negative, zero, or positive integer if `this.compareValue` is respectively less than, equal to, or
    * greater than `that.compareValue`.
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
    * Converts a string to its corresponding `FixedPoint` value.
    * @param value A decimal string.
    * @return `FixedPoint` with value represented by the string.
    */
  def apply(value: String): FixedPoint = new FixedPoint(new BigDecimal(value))

  /**
    * Number of decimal places after the decimal point stored for `FixedPoint` instances.
    */
  val computeScale = 40

  /**
    * Number of decimal places after the decimal points used to compare `FixedPoint` instances.
    */
  val compareScale = 20

  /**
    * Rounding mode used for intermediate computation and comparisons.
    */
  val roundingMode = RoundingMode.HALF_EVEN

  /**
    * Randomly generated offset that is added to `FixedPoint` instances before rounding for comparison. The use of
    * this random offset guarantees that the probability that a given `FixedPoint` lies close to a rounding cutoff is
    * small. In particular, this means that if a few digits of precision are lost, a `FixedPoint` will still almost
    * always hash into the same bucket.
    *
    * The correctness of code involving `FixedPoint` comparisons will often rely on the assumption that this probability
    * is effectively 0. This is a reasonable assumption if the sum of the number of digits of lost precision and the
    * order of magnitude of the number of `FixedPoint` operations performed is much smaller than the difference
    * `computeScale - compareScale`. If this assumption ever turns out to be false, the state of the program may become
    * corrupted.
    */
  val offset = {
    val zeros = "0" * compareScale
    val digits = List.fill(computeScale - compareScale)(Random.nextInt(10))
    new BigDecimal("0." + zeros + digits.mkString)
  }

  /**
    * `FixedPoint` with value closest to pi/2.
    */
  val HalfPi = {
    // Need computeScale + 2 digits because there is 1 digit before the decimal point, and we need 1 more digit at the
    // end to get the rounding right.
    val piValue = BigDecimalMath.pi(new MathContext(computeScale + 2, roundingMode))
    new FixedPoint(piValue.divide(new BigDecimal("2"), computeScale, roundingMode))
  }

  /**
    * `FixedPoint` with value closest to pi.
    */
  val Pi = {
    // Need computeScale + 1 digits because there is 1 digit before the decimal point.
    val piValue = BigDecimalMath.pi(new MathContext(computeScale + 1, roundingMode))
    new FixedPoint(piValue)
  }

  /**
    * `FixedPoint` with value closest to 3*pi/2.
    */
  val ThreeHalvesPi = {
    // Need computeScale + 2 digits because there is 1 digit before the decimal point, and we need 1 more digit at the
    // end to get the rounding right.
    val piValue = BigDecimalMath.pi(new MathContext(computeScale + 2, roundingMode))
    new FixedPoint(piValue.multiply(new BigDecimal("1.5")))
  }

  /**
    * `FixedPoint` with value closest to 2*pi.
    */
  val TwoPi = {
    // Need computeScale + 2 digits because there is 1 digit before the decimal point, and we need 1 more digit at the
    // end to get the rounding right.
    val piValue = BigDecimalMath.pi(new MathContext(computeScale + 2, roundingMode))
    new FixedPoint(piValue.multiply(new BigDecimal("2")))
  }

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
  val Two = new FixedPoint(new BigDecimal("2"))

  /**
    * Computes the angle theta whose tangent equals the ratio `y / x` and is in the same quadrant as `(y, x)`. Note that
    * this method differs from the standard definition of `atan2` methods in that it computes the result to be in the
    * range [0,2*pi).
    * @param y Y-coordinate.
    * @param x X-coordinate.
    * @return The two argument arc tangent of `y` and `x` in the range [0,2*pi).
    * @throws ArithmeticException If `y` and `x` are both zero.
    */
  def atan2(y: FixedPoint, x: FixedPoint): FixedPoint = {
    // signX and signY have the same signs as x and y, respectively
    val signX = x.compare(Zero)
    val signY = y.compare(Zero)
    // x positive
    if(signX > 0) {
      // atan returns something in the range [0,pi/2)
      if(signY >= 0) new FixedPoint(BigDecimalMath.atan((y / x).value))
      // atan returns something in the range (-pi/2,0), so we add 2*pi to be in the range (3*pi/2,2*pi)
      else new FixedPoint(BigDecimalMath.atan((y / x).value)) + TwoPi
    }
    // x negative
    else if(signX < 0) {
      // atan returns the arc tangent of the opposite angle, which is in the range (-pi/2,pi/2)
      // Adding pi gives us the angle we want and puts it in the range (pi/2,3*pi/2)
      new FixedPoint(BigDecimalMath.atan((y / x).value)) + Pi
    }
    // x zero
    else {
      // A vertical line pointing upward
      if(signY > 0) HalfPi
      // A vertical line pointing downward
      else if(signY < 0) ThreeHalvesPi
      // Undefined
      else throw new ArithmeticException("atan2(0,0)")
    }
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
    * Computes the square root of the argument.
    * @param x Nonnegative argument whose square root is to be computed.
    * @return Nonnegative square root of the argument.
    * @throws ArithmeticException If `x` is negative.
    */
  def sqrt(x: FixedPoint): FixedPoint = {
    val sign = x.compare(Zero)
    if(sign < 0) throw new ArithmeticException("square root of negative number")
    else if(sign == 0) Zero
    else new FixedPoint(BigDecimalMath.sqrt(x.value))
  }

  /**
    * Computes the sine of the argument.
    *
    * Warning: this implementation is bugged. This may throw an `IllegalArgumentException` on inputs very close to
    * multiples of pi/2 due to a bug in `BigDecimalMath`. This may be addressed in the future by using our own
    * implementation for those inputs. As a precaution, this method has special cases for `FixedPoint`s that compare as
    * equal to a multiple of pi/2. Nevertheless, this "fix" is not guaranteed to work.
    * @param theta Angle whose sine is to be computed, in radians.
    * @return Sine of the argument.
    */
  def sin(theta: FixedPoint): FixedPoint = {
    val normalized = mod2Pi(theta)
    if(normalized == Zero || normalized == Pi) Zero
    else if(normalized == HalfPi) One
    else if(normalized == ThreeHalvesPi) -One
    else new FixedPoint(BigDecimalMath.sin(theta.value))
  }

  /**
    * Computes the cosine of the argument.
    *
    * Warning: this implementation is bugged. This may throw an `IllegalArgumentException` on inputs very close to
    * multiples of pi/2 due to a bug in `BigDecimalMath`. This may be addressed in the future by using our own
    * implementation for those inputs. As a precaution, this method has special cases for `FixedPoint`s that compare as
    * equal to a multiple of pi/2. Nevertheless, this "fix" is not guaranteed to work.
    * @param theta Angle whose cosine is to be computed, in radians.
    * @return Cosine of the argument.
    */
  def cos(theta: FixedPoint): FixedPoint = {
    sin(theta + HalfPi)
  }
}
