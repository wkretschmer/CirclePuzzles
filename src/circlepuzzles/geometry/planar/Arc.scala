package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{PlanarGeometry, Angle}

class Arc extends PlanarGeometry.BaseArc {
  override def circle: Circle = ???

  override def rotate(hasCenter: Point, angle: Angle): Arc = ???

  override def startPoint: Point = ???

  override def endPoint: Point = ???

  override def midPoint: Point = ???

  override def union(that: Arc): Option[Arc] = ???
}
