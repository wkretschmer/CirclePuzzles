package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, SphericalGeometry}

class Arc extends SphericalGeometry.BaseArc {
  override def circle: Circle = ???

  override def rotate(rotationCenter: Point, angle: Angle): Arc = ???

  override def join(that: Arc): Option[Arc] = ???

  override def startPoint: Point = ???

  override def endPoint: Point = ???

  override def midPoint: Point = ???
}
