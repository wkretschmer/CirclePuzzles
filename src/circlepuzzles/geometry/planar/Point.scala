package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{PlanarGeometry, Angle}
import circlepuzzles.math.FixedPoint

case class Point(x: FixedPoint, y: FixedPoint) extends PlanarGeometry.BasePoint {
  override def rotate(center: Point, angle: Angle): Point = {
    val dx = x - center.x
    val dy = y - center.y
    val cos = angle.cos
    val sin = angle.sin
    val newdx = cos * dx - sin * dy
    val newdy = sin * dx + cos * dy
    Point(center.x + newdx, center.y + newdy)
  }
}
