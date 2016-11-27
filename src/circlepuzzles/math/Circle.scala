package circlepuzzles.math

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
    val dist = FixedPoint.sqrt(distSquared)
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
      // The cosine of half of the angle that spans the intersection; follows from law of cosines
      val cosHalfTheta = (distSquared + radius.pow(2) - that.radius.pow(2)) / (FixedPoint.Two * radius * dist)
      val halfTheta = FixedPoint.acos(cosHalfTheta).abs
      // Compute the angle to the center of the other circle
      val angleToCenter = FixedPoint.atan2(dy, dx)
      // The arc runs between angleToCenter +- halfTheta
      UnitArcs(angleToCenter - halfTheta, halfTheta * FixedPoint.Two)
    }
  }

  /**
    * Compute the set of nondegenerate intersections between the boundaries of this circle and the other circle. This is
    * to say that if the circles do not intersect or are equal, this returns the empty set. The result is expressed as a
    * set of angles with respect to the center of this circle.
    * @param that Circle whose boundary is intersected with the boundary of this circle.
    * @return A set containing all angles with respect to this circle's center at which the boundary of this circle
    * nondegenerately intersects the boundary of the argument. The returned angles are in the range [0,2*pi).
    */
  def intersections(that: Circle): Set[FixedPoint] = {
    // TODO: it would be nice to remove some of the duplicate code between this and intersectionArc
    // Distances between x and y coordinates
    val dx = that.x - x
    val dy = that.y - y
    // Distance between centers
    val distSquared = dx.pow(2) + dy.pow(2)
    val dist = FixedPoint.sqrt(distSquared)
    // Compare the distance between centers to the sum and difference of radii
    val distToSum = dist.compare(radius + that.radius)
    val distToDiff = dist.compare((radius - that.radius).abs)
    if(distToSum > 0 || distToDiff < 0 || that == this) { // Circles don't intersect or are equal
      Set.empty
    }
    else if(distToSum == 0 || distToDiff == 0) { // Circles intersect at one point
      // The point of intersection is at the angle between the centers
      Set(FixedPoint.mod2Pi(FixedPoint.atan2(dy, dx)))
    }
    else{
      // The cosine of half of the angle that spans the intersection; follows from law of cosines
      val cosHalfTheta = (distSquared + radius.pow(2) - that.radius.pow(2)) / (FixedPoint.Two * radius * dist)
      val halfTheta = FixedPoint.acos(cosHalfTheta).abs
      // Compute the angle to the center of the other circle
      val angleToCenter = FixedPoint.atan2(dy, dx)
      // The intersections are angleToCenter +- halfTheta
      Set(FixedPoint.mod2Pi(angleToCenter - halfTheta), FixedPoint.mod2Pi(angleToCenter + halfTheta))
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
    val cos = FixedPoint.cos(angle)
    val sin = FixedPoint.sin(angle)
    val newdx = cos * dx - sin * dy
    val newdy = sin * dx + cos * dy
    Circle(rx + newdx, ry + newdy, radius)
  }
}
