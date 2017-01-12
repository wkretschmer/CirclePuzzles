package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{PlanarGeometry, Angle}
import circlepuzzles.math.FixedPoint

/**
  * Immutable circles in the Euclidean plane.
  * @param center Center of this circle.
  * @param radius Positive radius of this circle.
  */
case class Circle(center: Point, radius: FixedPoint) extends PlanarGeometry.BaseCircle {
  override def rotate(rotationCenter: Point, angle: Angle): Circle = {
    // Rotate this by rotating the center, returning a circle with the same radius
    Circle(center.rotate(rotationCenter, angle), radius)
  }

  override def emptyArcs: ArcsOnCircle = ???

  override def fullArcs: ArcsOnCircle = ???
}
