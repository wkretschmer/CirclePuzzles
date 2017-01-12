package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{PlanarGeometry, Angle}

/**
  * Arcs around a circle in the Euclidean plane.
  */
class ArcsOnCircle extends PlanarGeometry.BaseArcsOnCircle {
  override def circle: Circle = ???

  override def rotate(rotationCenter: Point, angle: Angle): ArcsOnCircle = ???

  override def sameCircleUnion(that: ArcsOnCircle): ArcsOnCircle = ???

  override def sameCircleDifference(that: ArcsOnCircle): ArcsOnCircle = ???

  override def intersection(disk: Disk): ArcsOnCircle = ???

  override def nonEmpty: Boolean = ???
}
