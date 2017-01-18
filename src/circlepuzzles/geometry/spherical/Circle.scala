package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, SphericalGeometry}

class Circle extends SphericalGeometry.BaseCircle {
  override def center: Point = ???

  override def rotate(rotationCenter: Point, angle: Angle): Circle = ???

  override def emptyArcs: ArcsOnCircle = ???

  override def fullArcs: ArcsOnCircle = ???
}
