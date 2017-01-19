package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, SphericalGeometry}
import circlepuzzles.math.FixedPoint

/**
  * Points on the unit sphere. Points are represented by their (x, y, z) coordinates. Requires x^2^ + y^2^ + z^2^ = 1.
  * @param x X-coordinate of this point.
  * @param y Y-coordinate of this point.
  * @param z Z-coordinate of this point.
  */
case class Point(x: FixedPoint, y: FixedPoint, z: FixedPoint) extends SphericalGeometry.BasePoint {
  override def rotate(rotationCenter: Point, angle: Angle): Point = {
    // See Wikipedia: https://en.wikipedia.org/wiki/Rotation_matrix#Rotation_matrix_from_axis_and_angle
    // Sine and cosine of the angle
    val sin = angle.sin
    val cos = angle.cos
    val oneMinusCos = FixedPoint.One - cos
    // a * b * (1 - cos) for all a != b in (x, y, z)
    val xyOneMinusCos = rotationCenter.x * rotationCenter.y * oneMinusCos
    val yzOneMinusCos = rotationCenter.y * rotationCenter.z * oneMinusCos
    val zxOneMinusCos = rotationCenter.z * rotationCenter.x * oneMinusCos
    // a * sin for all a in (x, y, z)
    val xSin = rotationCenter.x * sin
    val ySin = rotationCenter.y * sin
    val zSin = rotationCenter.z * sin
    // Each line corresponds to one row of the rotation matrix
    val newX =
      (cos + rotationCenter.x.pow(2) * oneMinusCos) * x + (xyOneMinusCos - zSin) * y + (zxOneMinusCos + ySin) * z
    val newY =
      (xyOneMinusCos + zSin) * x + (cos + rotationCenter.y.pow(2) * oneMinusCos) * y + (yzOneMinusCos - xSin) * z
    val newZ =
      (zxOneMinusCos - ySin) * x + (yzOneMinusCos + xSin) * y + (cos + rotationCenter.z.pow(2) * oneMinusCos) * z
    Point(newX, newY, newZ)
  }
}
