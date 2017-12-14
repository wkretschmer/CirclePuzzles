package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, SphericalGeometry}
import circlepuzzles.math.FixedPoint

/**
  * Arcs on unit sphere. This represents a nonempty closed arc that spans from `startPoint` to `endPoint` in the
  * counterclockwise direction about `center`. Note that this representation is not unique; it is equivalent to an arc
  * with opposite center and switched start / end points.
  *
  * This requires that the circles through `startPoint` and `endPoint` centered at `center` be equal.
  * @param center Center of the circle to which this arc belongs.
  * @param startPoint Point at which this arc starts.
  * @param endPoint Point at which this arc ends.
  */
class Arc(override val center: Point, override val startPoint: Point, override val endPoint: Point)
  extends SphericalGeometry.BaseArc {

  override def circle: Circle = {
    new Circle(center, center.toVector3D.convexAngle(startPoint.toVector3D))
  }

  override def rotate(rotationCenter: Point, angle: Angle): Arc = {
    new Arc(
      center.rotate(rotationCenter, angle),
      startPoint.rotate(rotationCenter, angle),
      endPoint.rotate(rotationCenter, angle)
    )
  }

  override def join(that: Arc): Option[Arc] = {
    // Note: we don't compare the radii because we do this implicitly when comparing the start / end points
    // If the circles have the same center, the counterclockwise direction is the same
    if(center == that.center) {
      // If this ends where that starts, use this start point and that end point
      if(endPoint == that.startPoint) Some(new Arc(center, startPoint, that.endPoint))
      // Otherwise this starts where that ends; use that start point and this end point
      else Some(new Arc(center, that.startPoint, endPoint))
    }
    // Otherwise, the circles might be the same but have opposite representations
    // In this case, the counterclockwise direction is opposite
    else if(-center == that.center) {
      // If this ends where that ends, use this start point and that start point
      if(endPoint == that.endPoint) Some(new Arc(center, startPoint, that.startPoint))
      // Otherwise this starts where that starts, use that end point and this end point
      else Some(new Arc(center, that.endPoint, endPoint))
    }
    // Otherwise, the circles can't be equal
    else None
  }

  override def midPoint: Point = {
    // Compute the angle between start and end points with respect to the center
    val angle = center.angle(startPoint, endPoint)
    // Halve the angle
    val halfAngle = new Angle(angle.radians / FixedPoint.Two)
    // Rotate the start point about the center by the halved angle
    startPoint.rotate(center, halfAngle)
  }

  override def equals(that: Any): Boolean = {
    that match {
      case thatArc: Arc =>
        // If centers are equal, start and end points must be the same
        if(center == thatArc.center) {
          startPoint == thatArc.startPoint && endPoint == thatArc.endPoint
        }
        // Otherwise, if centers are opposite, start and end points must be opposite
        else if(-center == thatArc.center) {
          startPoint == thatArc.endPoint && endPoint == thatArc.startPoint
        }
        // Otherwise the arcs can't be equal; they don't belong to equal circles
        else false
      case _ =>
        false
    }
  }

  override def hashCode: Int = {
    // Because there are two distinct representations, we need to hash both of them
    (center, startPoint, endPoint).hashCode + (-center, endPoint, startPoint).hashCode
  }
}
