package circlepuzzles.geometry

import circlepuzzles.math.FixedPoint

/**
  * Angles that can specify rotations.
  * @param radians Value of the angle in radians. Must be in the range [0,2*pi).
  */
class Angle(val radians: FixedPoint) {
  /**
    * The sine of this angle.
    * @return `sin(radians)`.
    */
  def sin: FixedPoint = FixedPoint.sin(radians)

  /**
    * The cosine of this angle.
    * @return `cos(radians)`.
    */
  def cos: FixedPoint = FixedPoint.cos(radians)

  /**
    * Angle addition.
    * @param that Angle to add
    * @return Sum of the angles `this + that`.
    */
  def +(that: Angle): Angle = {
    new Angle(FixedPoint.mod2Pi(radians + that.radians))
  }

  /**
    * The explement of this angle, i.e. the angle that can be added to this to give a full circle.
    * @return Explement of this angle.
    */
  def explement: Angle = {
    new Angle(FixedPoint.TwoPi - radians)
  }

  /**
    * The supplement of this angle, i.e. the angle that can be added to this to give a half circle.
    * @return Supplement of this angle.
    */
  def supplement: Angle = {
    new Angle(FixedPoint.mod2Pi(FixedPoint.Pi - radians))
  }

  /**
    * Memoize this angle.
    * @return A memoized angle equivalent to `this`.
    */
  def memoized: MemoizedAngle = {
    new MemoizedAngle(radians)
  }

  /**
    * Tests if `that` is an equal `Angle`. Two angles are equal if and only if their values in radians are equal.
    * @param that Object to compare to.
    * @return True if and only if `this` and `that` are equal angles.
    */
  override def equals(that: Any): Boolean = {
    that match {
      case thatAngle: Angle => radians == thatAngle.radians
      case _ => false
    }
  }

  /**
    * Returns a hash code that satisfies the [[Object]] contract, which is to say that the hash depends only on
    * `radians`.
    * @return A hash code depending only on `radians`.
    */
  override def hashCode: Int = radians.hashCode

  /**
    * Returns a decimal representation of this angle's measure, in radians.
    * @return A string representation of this angle.
    */
  override def toString: String = s"Angle($radians)"
}

/**
  * Angles that memoize the corresponding sine and cosine values.
  * @param radians Value of the angle in radians. Must be in the range `[0,2*pi)`.
  */
class MemoizedAngle(radians: FixedPoint) extends Angle(radians) {
  /**
    * The sine of this angle. Memoized.
    */
  override val sin = super.sin

  /**
    * The cosine of this angle. Memoized.
    */
  override val cos = super.cos

  override def memoized: MemoizedAngle = this
}
