package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{Angle, PlanarGeometry}
import circlepuzzles.math.FixedPoint

/**
  * Arcs on the plane. This represents a nonempty closed arc that spans from `start` to `end` in the counterclockwise
  * direction.
  *
  * If `start < end`, the arc has angle `end - start`. If `start == end`, represents a full circle. If `start > end`,
  * the arc has angle `2*pi - (start - end).`
  * @param circle Circle to which this arc belongs.
  * @param start Angle at which this arc begins.
  * @param end Angle at which this arc ends.
  */
case class Arc(circle: Circle, start: Angle, end: Angle) extends PlanarGeometry.BaseArc {
  override def rotate(rotationCenter: Point, angle: Angle): Arc = {
    val rotatedCircle = circle.rotate(rotationCenter, angle)
    val rotatedStart = new Angle(FixedPoint.mod2Pi(start.radians + angle.radians))
    val rotatedEnd = new Angle(FixedPoint.mod2Pi(end.radians + angle.radians))
    Arc(rotatedCircle, rotatedStart, rotatedEnd)
  }

  // Memoized
  override val startPoint: Point = circle.pointAtAngle(start)

  // Memoized
  override val endPoint: Point = circle.pointAtAngle(end)

  override def midPoint: Point = {
    val meanAngle = (start.radians + end.radians) / FixedPoint.Two
    // We add pi to get the actual mid angle if the arc wraps around zero
    val midAngle =
      if(end.radians > start.radians) new Angle(meanAngle)
      else new Angle(FixedPoint.mod2Pi(meanAngle + FixedPoint.Pi))
    circle.pointAtAngle(midAngle)
  }

  override def join(that: Arc): Option[Arc] = {
    // Arcs only joinable if they belong to the same circle
    if(circle == that.circle) {
      // If this ends where that starts, use this start point and that endpoint
      if(end == that.start) Some(Arc(circle, start, that.end))
      // If this starts where that ends, use that start point and this endpoint
      else if(start == that.end) Some(Arc(circle, that.start, end))
      // Otherwise arcs can't be joined
      else None
    }
    else None
  }
}
