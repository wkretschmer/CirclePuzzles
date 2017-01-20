package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, SphericalGeometry}
import circlepuzzles.math.UnitArcs

class ArcsOnCircle(override val circle: Circle, val zero: Point, val unitArcs: UnitArcs) extends SphericalGeometry.BaseArcsOnCircle {
  override def rotate(rotationCenter: Point, angle: Angle): ArcsOnCircle = {
    // Rotate both the circle and the zero point, but not the unitArcs because they are with respect to the zero point
    new ArcsOnCircle(circle.rotate(rotationCenter, angle), zero.rotate(rotationCenter, angle), unitArcs)
  }

  override def intersection(disk: Disk): ArcsOnCircle = ???

  override def sameCircleDifference(that: ArcsOnCircle): ArcsOnCircle = ???

  override def sameCircleUnion(that: ArcsOnCircle): ArcsOnCircle = ???

  override def nonEmpty: Boolean = {
    unitArcs.nonEmpty
  }
}
