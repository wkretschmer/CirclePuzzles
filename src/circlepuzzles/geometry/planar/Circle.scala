package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{Angle, PlanarGeometry}
import circlepuzzles.math.{FixedPoint, UnitArcs}

/**
  * Circles in the Euclidean plane.
  * @param center Center of this circle.
  * @param radius Positive radius of this circle.
  */
case class Circle(center: Point, radius: FixedPoint) extends PlanarGeometry.BaseCircle {
  override def rotate(rotationCenter: Point, angle: Angle): Circle = {
    // Rotate this by rotating the center, returning a circle with the same radius
    Circle(center.rotate(rotationCenter, angle), radius)
  }

  override def emptyArcs: ArcsOnCircle = {
    new ArcsOnCircle(this, UnitArcs.Empty)
  }

  override def fullArcs: ArcsOnCircle = {
    new ArcsOnCircle(this, UnitArcs.FullCircle)
  }

  /**
    * Computes the point on this circle at the specified angle, relative to the direction of the positive x-axis.
    * @param angle Angle in the counterclockwise direction
    * @return Point on this circle at the specified angle.
    */
  def pointAtAngle(angle: Angle): Point = {
    val newX = center.x + radius * angle.cos
    val newY = center.y + radius * angle.sin
    Point(newX, newY)
  }
}
