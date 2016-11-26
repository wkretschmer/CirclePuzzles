package circlepuzzles.puzzle

import java.math.BigDecimal

import circlepuzzles.math._

/**
  * Atomic rotations for circle puzzles. By convention, one application of a move rotates the interior of its circle in
  * the counterclockwise direction.
  * @param circle The circle whose interior this move acts on by rotation about the circle's center.
  * @param increment The fraction of 2*pi by which this move rotates. Equivalently, this is the order of the move (i.e
  * the number of times it must be repeated to get the identity).
  */
case class Move(circle: Circle, increment: Int) {
  /**
    * The angle by which this move rotates. Measured in radians in the counterclockwise direction.
    */
  val angle = FixedPoint.TwoPi / new FixedPoint(new BigDecimal(increment))

  /**
    * All nonzero angles by which powers of this move can rotate. When the move is viewed as a rotation by `angle` of a
    * subset of the plane, these are the angles of the nontrivial powers of this rotation. So, this consists of the
    * angles `angle, ... , (increment - 1) * angle`.
    */
  val nonzeroAngles = List.iterate(angle, increment - 1)(_ + angle)
}
