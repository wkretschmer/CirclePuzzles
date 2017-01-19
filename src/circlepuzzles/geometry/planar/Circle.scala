package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{Angle, PlanarGeometry}
import circlepuzzles.math.{FixedPoint, UnitArcs}
import circlepuzzles.math.FixedPoint._

/**
  * Circles in the Euclidean plane. Disks in the plane are uniquely defined by circles, so this also subclasses the
  * `Disk` type.
  * @param center Center of this circle.
  * @param radius Positive radius of this circle.
  */
case class Circle(override val center: Point, radius: FixedPoint) extends PlanarGeometry.BaseCircle
  with PlanarGeometry.BaseDisk {

  override def circle: Circle = this

  override def rotate(rotationCenter: Point, angle: Angle): Circle = {
    // Rotate this by rotating the center, returning a circle with the same radius
    Circle(center.rotate(rotationCenter, angle), radius)
  }

  override def emptyArcs: ArcsOnCircle = {
    new ArcsOnCircle(this, UnitArcs.Empty)
  }

  override def fullArcs: ArcsOnCircle = {
    new ArcsOnCircle(this, UnitArcs.FullCircle)
  }

  override def containsCompare(pt: Point): Int = {
    // Compare the square of the distance to the center (i.e. dx^2 + dy^2) to the squared radius
    val dx = circle.center.x - pt.x
    val dy = circle.center.y - pt.y
    (dx.pow(2) + dy.pow(2)).compare(circle.radius.pow(2))
  }

  /**
    * Computes the point on this circle at the specified angle, relative to the direction of the positive x-axis.
    * @param angle Angle in the counterclockwise direction
    * @return Point on this circle at the specified angle.
    */
  def pointAtAngle(angle: Angle): Point = {
    val newX = center.x + radius * angle.cos
    val newY = center.y + radius * angle.sin
    Point(newX, newY)
  }

  /**
    * Compute the set of nondegenerate intersections between the boundaries of this circle and the other circle. This is
    * to say that if the circles do not intersect or are equal, this returns the empty set.
    * @param that Circle whose boundary is intersected with the boundary of this circle.
    * @return A set of pairs `(thisAngle, thatAngle)` indicating that the point on the boundary of `this` at `thisAngle`
    * from `this` circle's center intersects the boundary of the other circle, where `thatAngle` is the angle from the
    * center of `that` to the point of intersection. The returned angles are in the range [0,2*pi).
    */
  def intersections(that: Circle): Set[(FixedPoint, FixedPoint)] = {
    // TODO: it would be nice to remove some of the duplicate code between this and arcsOnCircle.intersection
    // TODO: should this return Angles instead of FixedPoints?
    // Distances between x and y coordinates
    val dx = that.center.x - center.x
    val dy = that.center.y - center.y
    // Distance between centers
    val distSquared = dx.pow(2) + dy.pow(2)
    val dist = sqrt(distSquared)
    // Compare the distance between centers to the sum and difference of radii
    val distToSum = dist.compare(radius + that.radius)
    val distToDiff = dist.compare((radius - that.radius).abs)
    if(distToSum > 0 || distToDiff < 0 || that == this) { // Circles don't intersect or are equal
      Set.empty
    }
    else if(distToSum == 0) { // Circles separate and intersect at one point
    // The point of intersection is at the angle between the centers
    val thisAngle = atan2Mod2Pi(dy, dx)
      // The angle from the other center differs from thisAngle by pi; they are opposite angles
      val thatAngle = mod2Pi(thisAngle + Pi)
      Set((thisAngle, thatAngle))
    }
    else if(distToDiff == 0) { // Smaller circle contained in larger circle and circles intersect at one point
    // The angle is the same from both centers
    // This angle is also the angle from the center of the larger to the smaller circle
    val angle = if(radius > that.radius) atan2Mod2Pi(dy, dx) else atan2Mod2Pi(-dy, -dx)
      Set((angle, angle))
    }
    else{
      val radiusSquared = radius.pow(2)
      // See http://paulbourke.net/geometry/circlesphere/
      val a = (distSquared + radiusSquared - that.radius.pow(2)) / (Two * dist)
      val h = sqrt(radiusSquared - a.pow(2))
      // (midX, midY) is the intersection of the line between the centers and the line between the circle intersections
      val midX = center.x + a * dx / dist
      val midY = center.y + a * dy / dist

      val newX1 = midX + h * dy / dist
      val newY1 = midY - h * dx / dist
      val thisAngle1 = atan2Mod2Pi(newY1 - center.y, newX1 - center.x)
      val thatAngle1 = atan2Mod2Pi(newY1 - that.center.y, newX1 - that.center.x)

      val newX2 = midX - h * dy / dist
      val newY2 = midY + h * dx / dist
      val thisAngle2 = atan2Mod2Pi(newY2 - center.y, newX2 - center.x)
      val thatAngle2 = atan2Mod2Pi(newY2 - that.center.y, newX2 - that.center.x)
      Set((thisAngle1, thatAngle1), (thisAngle2, thatAngle2))
    }
  }
}
