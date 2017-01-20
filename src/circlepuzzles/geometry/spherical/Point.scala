package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, Vector3D, SphericalGeometry}
import circlepuzzles.math.FixedPoint

/**
  * Points on the unit sphere. Points are represented by their (x, y, z) coordinates. Requires x^2^ + y^2^ + z^2^ = 1.
  * @param x X-coordinate of this point.
  * @param y Y-coordinate of this point.
  * @param z Z-coordinate of this point.
  */
case class Point(x: FixedPoint, y: FixedPoint, z: FixedPoint) extends SphericalGeometry.BasePoint {
  /**
    * Constructs a point from the given polar coordinates.
    * @param theta Azimuth angle on the XY plane.
    * @param phi Polar angle with respect to the Z axis. Does not necessarily have to be in the range [0,pi]. If the
    * angle is in the range (pi,2pi), this returns the point opposite `(theta, phi - pi)`.
    */
  def this(theta: Angle, phi: Angle) = {
    this(theta.cos * phi.sin, theta.sin * phi.sin, phi.cos)
  }

  /**
    * Constructs a point from the given 3D vector.
    * @param vector3D A vector with norm 1.
    */
  def this(vector3D: Vector3D) = {
    this(vector3D.x, vector3D.y, vector3D.z)
  }

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

  /**
    * Returns a representation of this point in polar coordinates.
    * @return Angles `(theta, phi)` where `theta` is the azimuth angle on the XY plane, and `phi` is the polar angle
    * with respect to the Z axis.
    */
  def toPolar: (Angle, Angle) = {
    val theta = FixedPoint.atan2Mod2Pi(y, x)
    val phi = FixedPoint.acos(z)
    (new Angle(theta), new Angle(phi))
  }

  /**
    * Returns a representation of this point as a vector in 3-space.
    * @return A vector whose coordinates are the same as corresponding coordinates of this point.
    */
  def toVector3D: Vector3D = {
    Vector3D(x, y, z)
  }

  /**
    * Returns the point opposite from this point on the unit sphere. This is equivalent to the entrywise negation of
    * this point.
    * @return Point opposite from this point.
    */
  def unary_- : Point = {
    Point(-x, -y, -z)
  }

  /**
    * Computes the angle of `end` from `start` in the counterclockwise direction about `this`. This can be viewed as the
    * angle that `start` must be rotated counterclockwise about `this` for `this`, `start`, and `end` to all lie on a
    * great circle. Therefore, the angle is signed in that `angle(start, end) + angle(end, start) = 2*pi` as a rule.
    * Returns 0 in the degenerate cases where `this = start` or `this = end`.
    * @param start Start point of the counterclockwise angle.
    * @param end End point of the counterclockwise angle.
    * @return Counterclockwise angle between `start` and `end` with respect to `this`.
    */
  def angle(start: Point, end: Point): Angle = {
    // Vectors equivalent to this, start, and end, respectively
    val this3D = toVector3D
    val start3D = start.toVector3D
    val end3D = end.toVector3D
    // Projections of start and end, respectively, onto the plane orthogonal to this
    // Subtract from the vectors their projection onto this, computed using formula proj_u(v) = u * ((v . u) / (u . u))
    // These are points on the unit sphere, so u . u = 1 by assumption
    val startProj = start3D - this3D * start3D.dotProduct(this3D)
    val endProj = end3D - this3D * end3D.dotProduct(this3D)
    // Angle between start and end, but in the range [0,pi]
    val convexAngle = startProj.convexAngle(endProj)
    // We use the right hand rule to figure out if we have the desired angle, or its explement (i.e. 2*pi - convexAngle)
    // The cross product of the projected vectors will be in the same direction as this if we have the desired angle
    val crossProduct = startProj.crossProduct(endProj)
    // Test if the cross product is in the same direction as this (or 0) by looking comparing signs entrywise
    val sameDirection = productIterator.zip(crossProduct.productIterator).forall {
      case (thisCoord: FixedPoint, prodCoord: FixedPoint) =>
        // Take the signs of corresponding coordinates
        val thisSign = thisCoord.signum
        val prodSign = prodCoord.signum
        // Either all entries must have the same signs, or the product must be the 0 vector
        prodSign == 0 || thisSign == prodSign
      case _ =>
        // TODO fail fast here?
        false
    }
    // Return the angle if the cross product is in the same direction
    if(sameDirection) convexAngle
    // Otherwise, return the explementary angle
    else convexAngle.explement
  }
}
