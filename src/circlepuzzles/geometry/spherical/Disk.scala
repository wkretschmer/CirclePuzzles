package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, SphericalGeometry}

class Disk extends SphericalGeometry.BaseDisk {
  override def circle: Circle = ???

  override def rotate(rotationCenter: Point, angle: Angle): Disk = ???

  override def containsCompare(pt: Point): Int = ???
}
