package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{PlanarGeometry, Angle}
import circlepuzzles.math.FixedPoint

/**
  * Immutable points in the Euclidean plane. Points are represented by their (x, y) coordinates.
  * @param x X-coordinate of this point.
  * @param y Y-coordinate of this point.
  */
case class Point(x: FixedPoint, y: FixedPoint) extends PlanarGeometry.BasePoint {
  override def rotate(rotationCenter: Point, angle: Angle): Point = {
    val dx = x - rotationCenter.x
    val dy = y - rotationCenter.y
    // cos and sin might not be cached, so we put them here for efficiency
    val cos = angle.cos
    val sin = angle.sin
    // Apply a 2 x 2 rotation matrix to the differences
    val newdx = cos * dx - sin * dy
    val newdy = sin * dx + cos * dy
    // Add the rotated differences back to the rotation center
    Point(rotationCenter.x + newdx, rotationCenter.y + newdy)
  }
}
