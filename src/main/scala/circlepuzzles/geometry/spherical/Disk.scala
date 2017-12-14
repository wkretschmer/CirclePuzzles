package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, SphericalGeometry}

/**
  * Disks on the unit sphere. A disk is defined as the set of points whose distance in radians to a given center point
  * is less than or equal to a given radius.
  * @param center Center of this disk.
  * @param radius Radius of this disk, in radians. Must be in the range (0,pi).
  */
case class Disk(override val center: Point, radius: Angle) extends SphericalGeometry.BaseDisk {
  override def circle: Circle = {
    new Circle(center, radius)
  }

  override def rotate(rotationCenter: Point, angle: Angle): Disk = {
    // Rotate this by rotating the center, returning a disk with the same radius
    Disk(center.rotate(rotationCenter, angle), radius)
  }

  override def containsCompare(pt: Point): Int = {
    // Compute the convex angle between the center and the point
    val angle = center.toVector3D.convexAngle(pt.toVector3D)
    // Compare the angle to the radius, e.g. if it's less than the radius then it belongs to the interior
    angle.radians.compare(radius.radians)
  }
}
