package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, SphericalGeometry}
import circlepuzzles.math.UnitArcs

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
    // TODO: is there a cheaper / more elegant way to do this? Does this hashCode work?
    (center, radius).hashCode + (-center, radius.supplement).hashCode
  }
}
