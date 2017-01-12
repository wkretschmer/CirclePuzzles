package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{PlanarGeometry, Angle}

/**
  * Immutable disks in the Euclidean plane. A disk is uniquely defined as the interior of a circle.
  * @param circle Boundary of this disk.
  */
case class Disk(circle: Circle) extends PlanarGeometry.BaseDisk {
  override def rotate(rotationCenter: Point, angle: Angle): Disk = {
    // Rotate the underlying circle
    Disk(circle.rotate(rotationCenter, angle))
  }

  override def containsCompare(pt: Point): Int = {
    // Compare the square of the distance to the center (i.e. dx^2 + dy^2) to the squared radius
    val dx = circle.center.x - pt.x
    val dy = circle.center.y - pt.y
    (dx.pow(2) + dy.pow(2)).compare(circle.radius.pow(2))
  }
}
