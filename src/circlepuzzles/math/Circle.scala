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
    val dx = that.x - x
    val dy = that.y - y
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
      // The cosine of half of the arc length of the intersection; follows from law of cosines
      val cosHalfTheta = (distSquared + radius.pow(2) - that.radius.pow(2)) / (FixedPoint.Two * radius * dist)
      val halfTheta = FixedPoint.acos(cosHalfTheta).abs
      // Compute the angle to the center of the other circle
      val angleToCenter = FixedPoint.atan2(dy, dx)
      // The arc runs between angleToCenter +- halfTheta
      UnitArcs(angleToCenter - halfTheta, halfTheta * FixedPoint.Two)
    }
  }
}
