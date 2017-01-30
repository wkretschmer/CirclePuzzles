package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, SphericalGeometry}
import circlepuzzles.math.{FixedPoint, UnitArcs}

/**
  * Circle on the unit sphere. A circle is defined as the set of points whose distance in radians to a given center
  * point is equal to a given radius. Note that this representation is not unique; it is equivalent to a circle with
  * opposite center and supplementary radius.
  * @param center Center of this circle.
  * @param radius Radius of this circle, in radians. Must be in the range (0,pi).
  */
class Circle(override val center: Point, val radius: Angle) extends SphericalGeometry.BaseCircle {
  override def rotate(rotationCenter: Point, angle: Angle): Circle = {
    // Rotate this by rotating the center, returning a circle with the same radius
    new Circle(center.rotate(rotationCenter, angle), radius)
  }

  /**
    * Returns a point on this circle.
    * @return A point on this circle.
    */
  def arbitraryPoint: Point = {
    // Compute the polar coordinates of the center
    val (theta, phi) = center.toPolar
    // Add the angle in the polar direction, and memoize so the constructor is more efficient
    new Point(theta.memoized, (phi + radius).memoized)
  }

  override def emptyArcs: ArcsOnCircle = {
    // Choose an arbitrary zero point
    new ArcsOnCircle(this, arbitraryPoint, UnitArcs.Empty)
  }

  override def fullArcs: ArcsOnCircle = {
    // Choose an arbitrary zero point
    new ArcsOnCircle(this, arbitraryPoint, UnitArcs.FullCircle)
  }

  /**
    * Computes all intersections with the other circle.
    * @param that Circle to intersect.
    * @return Points of intersection of `this` and `that`.
    */
  def intersections(that: Circle): Set[Point] = {
    // See https://gis.stackexchange.com/questions/48937/calculating-intersection-of-two-circles
    // We use the same variable names as above
    // x1 and x2 are the circle centers
    val x1 = center.toVector3D
    val x2 = that.center.toVector3D
    // q is the dot product of the centers, q2 is its square
    val q = x1.dotProduct(x2)
    val q2 = q.pow(2)
    // If q^2 = 1, then the centers are the same or opposite; no intersections
    if(q2 == FixedPoint.One) return Set()
    // Cosines of the two circle radii
    val r1cos = radius.cos
    val r2cos = that.radius.cos
    val oneMinusq2 = FixedPoint.One - q2
    // a and b are scalars that produce a linear combination of x1 and x2
    val a = (r1cos - q * r2cos) / oneMinusq2
    val b = (r2cos - q * r1cos) / oneMinusq2
    // x0 is this linear combination; it is the unique such point on the intersection of the planes through the circles
    val x0 = x1 * a + x2 * b
    val x02 = x0.normSquared
    val x02compareOne = x02.compare(FixedPoint.One)
    // If |x0| > 1, the intersection lies outside the sphere; no intersections
    if(x02compareOne > 0) {
      Set()
    }
    // If |x0| = 1, then x0 is the unique intersection because it lies on the sphere
    else if(x02compareOne == 0) {
      Set(new Point(x0))
    }
    // If |x0| < 1, there are two intersections
    else {
      // The two intersections differ from x0 by some scalar multiple of n, the cross product of x1 and x2
      val n = x1.crossProduct(x2)
      // The scaling factor on n is +-t
      val t = FixedPoint.sqrt((FixedPoint.One - x02) / n.normSquared)
      // Product of n by scalar multiple t
      val nt = n * t
      Set(new Point(x0 + nt), new Point(x0 - nt))
    }
  }

  override def equals(that: Any): Boolean = {
    that match {
      case thatCircle: Circle =>
        // Circles can be equal in two cases:
        // Either they have the same center and radii
        if(center == thatCircle.center) {
          radius == thatCircle.radius
        }
        // Or have opposite centers and supplementary radii
        else if(-center == thatCircle.center) {
          radius == thatCircle.radius.supplement
        }
        // Otherwise they are not equal
        else false
      case _ =>
        false
    }
  }

  override def hashCode: Int = {
    // Because there are two distinct representations, we need to hash both of them
    (center, radius).hashCode + (-center, radius.supplement).hashCode
  }
}
