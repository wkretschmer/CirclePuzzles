package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{PlanarGeometry, Angle}
import circlepuzzles.math.FixedPoint

case class Circle(center: Point, radius: FixedPoint) extends PlanarGeometry.BaseCircle {
  override def rotate(rotationCenter: Point, angle: Angle): Circle = {
    Circle(center.rotate(rotationCenter, angle), radius)
  }

  override def emptyArcs: ArcsOnCircle = ???

  override def fullArcs: ArcsOnCircle = ???
}
