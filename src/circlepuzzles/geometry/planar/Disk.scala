package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{PlanarGeometry, Angle}

class Disk extends PlanarGeometry.BaseDisk {
  override def circle: Circle = ???

  override def rotate(rotationCenter: Point, angle: Angle): Disk = ???

  override def contains(pt: Point): Boolean = ???

  override def strictlyContains(pt: Point): Boolean = ???
}
