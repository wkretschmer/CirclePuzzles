package circlepuzzles.math

import FixedPoint._

/**
  * Immutable circles on the Euclidean plane.
  * @param x X-coordinate of this circle's center.
  * @param y Y-coordinate of this circle's center.
  * @param radius Positive radius of this circle.
  */
case class Circle(x: FixedPoint, y: FixedPoint, radius: FixedPoint) {
  /**
    * Compute the set of points on the boundary of this circle that intersect the interior of the other circle.
    * If the circles do not intersect or are tangent at a single point, returns an empty arc.
    * If `this` is contained in `that`, returns a full circle, including the case with a single point of tangency.
    * If `that` is contained in `this` or both circles are equal, returns an empty arc.
    * Otherwise, the solution is a nondegenerate arc between two points.
    * @param that Circle whose interior is intersected with the boundary of this circle.
    * @return The arc on this circle's boundary that intersects the interior of the other circle.
    */
  def intersectionArc(that: Circle): UnitArcs = {
    // Distances between x and y coordinates
    val dx = that.x - x
    val dy = that.y - y
    // Distance between centers
    val distSquared = dx.pow(2) + dy.pow(2)
    val dist = sqrt(distSquared)
    if(dist >= radius + that.radius) { // Circles don't intersect or are tangent at one point
      UnitArcs.Empty
    }
    else if(dist <= (radius - that.radius).abs) { // Circle of smaller radius contained in the other
      if(this.radius < that.radius) { // this contained in that
        UnitArcs.FullCircle
      }
      else { // that contained in this, or circles are equal
        UnitArcs.Empty
      }
    }
    else {
      val radiusSquared = radius.pow(2)
      // See http://paulbourke.net/geometry/circlesphere/
      val a = (distSquared + radiusSquared - that.radius.pow(2)) / (Two * dist)
      val h = sqrt(radiusSquared - a.pow(2))
      // (midX, midY) is the intersection of the line between the centers and the line between the circle intersections
      val midX = x + a * dx / dist
      val midY = y + a * dy / dist

      val newX1 = midX + h * dy / dist
      val newY1 = midY - h * dx / dist
      val angle1 = mod2Pi(atan2(newY1 - y, newX1 - x))

      val newX2 = midX - h * dy / dist
      val newY2 = midY + h * dx / dist
      val angle2 = mod2Pi(atan2(newY2 - y, newX2 - x))
      // It is not at all obvious that the arc necessarily starts at angle1 and ends at angle2. One can verify it by
      // considering the possible signs of dx and dy.
      UnitArcs(angle1, angle2)
    }
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
    // TODO: it would be nice to remove some of the duplicate code between this and intersectionArc
    // Distances between x and y coordinates
    val dx = that.x - x
    val dy = that.y - y
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
      val thisAngle = mod2Pi(atan2(dy, dx))
      // The angle from the other center differs from thisAngle by pi; they are opposite angles
      val thatAngle = mod2Pi(thisAngle + Pi)
      Set((thisAngle, thatAngle))
    }
    else if(distToDiff == 0) { // Smaller circle contained in larger circle and circles intersect at one point
      // The angle is the same from both centers
      // This angle is also the angle from the center of the larger to the smaller circle
      val angle = if(radius > that.radius) mod2Pi(atan2(dy, dx)) else mod2Pi(atan2(-dy, -dx))
      Set((angle, angle))
    }
    else{
      val radiusSquared = radius.pow(2)
      // See http://paulbourke.net/geometry/circlesphere/
      val a = (distSquared + radiusSquared - that.radius.pow(2)) / (Two * dist)
      val h = sqrt(radiusSquared - a.pow(2))
      // (midX, midY) is the intersection of the line between the centers and the line between the circle intersections
      val midX = x + a * dx / dist
      val midY = y + a * dy / dist

      val newX1 = midX + h * dy / dist
      val newY1 = midY - h * dx / dist
      val thisAngle1 = mod2Pi(atan2(newY1 - y, newX1 - x))
      val thatAngle1 = mod2Pi(atan2(newY1 - that.y, newX1 - that.x))

      val newX2 = midX - h * dy / dist
      val newY2 = midY + h * dx / dist
      val thisAngle2 = mod2Pi(atan2(newY2 - y, newX2 - x))
      val thatAngle2 = mod2Pi(atan2(newY2 - that.y, newX2 - that.x))
      Set((thisAngle1, thatAngle1), (thisAngle2, thatAngle2))
    }
  }

  /**
    * Compute the rotation of this circle about an arbitrary point in the plane.
    * @param rx X-coordinate of rotation center.
    * @param ry Y-coordinate of rotation center.
    * @param angle Rotation angle, in radians.
    * @return The rotation of this circle about the specified point by the specified angle.
    */
  def rotate(rx: FixedPoint, ry: FixedPoint, angle: FixedPoint): Circle = {
    val dx = x - rx
    val dy = y - ry
    val cosAngle = cos(angle)
    val sinAngle = sin(angle)
    val newdx = cosAngle * dx - sinAngle * dy
    val newdy = sinAngle * dx + cosAngle * dy
    Circle(rx + newdx, ry + newdy, radius)
  }

  /**
    * Tests if this circle strictly contains `(xx, yy)`.
    * @param xx X-coordinate of point.
    * @param yy Y-coordinate of point.
    * @return True if and only if the point specified by the arguments is in the interior of this circle.
    */
  def strictlyContains(xx: FixedPoint, yy: FixedPoint): Boolean = {
    val dx = x - xx
    val dy = y - yy
    dx.pow(2) + dy.pow(2) < radius.pow(2)
  }
}
