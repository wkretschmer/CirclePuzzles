package circlepuzzles.math

import FixedPoint._

/**
  * Immutable points in the Euclidean plane. Points are represented by their (x, y) coordinates.
  * @param x X-coordinate of this point.
  * @param y Y-coordinate of this point.
  */
case class FixedPoint2D(x: FixedPoint, y: FixedPoint) {
  /**
    * Rotate this point around the other point.
    * @param center Center of rotation.
    * @param angle Angle of rotation, in radians.
    * @return The rotation of this point about the other point by the specified angle in the counterclockwise direction.
    */
  def rotate(center: FixedPoint2D, angle: FixedPoint): FixedPoint2D = {
    rotate(center, sin(angle), cos(angle))
  }

  /**
    * Rotate this point around the other point.
    * @param center Center of rotation.
    * @param sin Sine of the angle of rotation.
    * @param cos Cosine of the angle rotation.
    * @return The rotation of this point about the other point by the specified angle in the counterclockwise direction.
    */
  def rotate(center: FixedPoint2D, sin: FixedPoint, cos: FixedPoint): FixedPoint2D = {
    val dx = x - center.x
    val dy = y - center.y
    val newdx = cos * dx - sin * dy
    val newdy = sin * dx + cos * dy
    FixedPoint2D(center.x + newdx, center.y + newdy)
  }
}
