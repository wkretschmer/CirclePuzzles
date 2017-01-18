package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, SphericalGeometry}

class ArcsOnCircle extends SphericalGeometry.BaseArcsOnCircle {
  override def circle: Circle = ???

  override def rotate(rotationCenter: Point, angle: Angle): ArcsOnCircle = ???

  override def intersection(disk: Disk): ArcsOnCircle = ???

  override def sameCircleDifference(that: ArcsOnCircle): ArcsOnCircle = ???

  override def sameCircleUnion(that: ArcsOnCircle): ArcsOnCircle = ???

  override def nonEmpty: Boolean = ???
}
