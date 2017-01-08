package circlepuzzles.puzzle

import circlepuzzles.geometry.{MemoizedAngle, HasGeometry}
import circlepuzzles.math.FixedPoint

/**
  * Moves for a puzzle in a particular geometry.
  */
trait GeometricMove extends HasGeometry {
  import geom._

  /**
    * Atomic rotations for circle puzzles. By convention, one application of a move rotates its disk in the
    * counterclockwise direction.
    * @param disk Disk whose interior this move acts on by rotation about its center.
    * @param increment Fraction of 2*pi by which this move rotates. Equivalently, this is the order of the move (i.e
    * the number of times it must be repeated to get the identity).
    */
  case class Move(disk: Disk, increment: Int) {
    /**
      * The angle by which this move rotates.
      */
    val angle = new MemoizedAngle(FixedPoint.TwoPi / FixedPoint(increment))

    /**
      * All nonzero angles by which powers of this move can rotate. When the move is viewed as a rotation by `angle` of
      * a subset of the geometry, these are the angles of the nontrivial powers of this rotation. So, this consists of
      * the angles with value `angle.radians, ... , (increment - 1) * angle.radians`.
      */
    val nonzeroAngles = List.iterate(angle, increment - 1)(a => new MemoizedAngle(a.radians + angle.radians))
  }
}
