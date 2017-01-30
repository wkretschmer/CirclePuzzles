package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, SphericalGeometry}
import circlepuzzles.math.{FixedPoint, UnitArcs}

/**
  * Collections of arcs around a circle on the unit sphere.
  * @param circle Circle to which these arcs belong.
  * @param zero An arbitrary point of reference; must belong to the circle.
  * @param unitArcs Arcs that belong to this collection, measured as angles in the counterclockwise direction about the
  * circle's center, starting at the zero point given.
  */
class ArcsOnCircle(override val circle: Circle, val zero: Point, val unitArcs: UnitArcs)
  extends SphericalGeometry.BaseArcsOnCircle {

  override def rotate(rotationCenter: Point, angle: Angle): ArcsOnCircle = {
    // Rotate both the circle and the zero point, but not the unitArcs because they are with respect to the zero point
    new ArcsOnCircle(circle.rotate(rotationCenter, angle), zero.rotate(rotationCenter, angle), unitArcs)
  }

  override def intersection(disk: Disk): ArcsOnCircle = {
    // See https://gis.stackexchange.com/questions/48937/calculating-intersection-of-two-circles
    // We use the same variable names as above
    // x1 and x2 are the circle centers
    val x1 = center.toVector3D
    val x2 = disk.center.toVector3D
    // q is the dot product of the centers, q2 is its square
    val q = x1.dotProduct(x2)
    val q2 = q.pow(2)
    // If q^2 = 1, then the centers are the same or opposite; no intersections
    if(q2 == FixedPoint.One) {
      val diskContainsCircle =
        // If the centers are the same, the disk contains the circle if its radius is greater
        if(center == disk.center) disk.radius.radians > circle.radius.radians
        // If the centers are opposite, the disk contains the circle if its radius is greater than the supplement
        else disk.radius.radians > circle.radius.supplement.radians
      // If the disk contains the circle, it contains all of this
      if(diskContainsCircle) return this
      // Otherwise, return empty arc
      else return new ArcsOnCircle(circle, zero, UnitArcs.Empty)
    }

    // Cosines of the two circle radii
    val r1cos = circle.radius.cos
    val r2cos = disk.radius.cos
    val oneMinusq2 = FixedPoint.One - q2
    // a and b are scalars that produce a linear combination of x1 and x2
    val a = (r1cos - q * r2cos) / oneMinusq2
    val b = (r2cos - q * r1cos) / oneMinusq2
    // x0 is this linear combination; it is the unique such point on the intersection of the planes through the circles
    val x0 = x1 * a + x2 * b
    val x02 = x0.normSquared

    // If |x0| >= 1, the intersection lies outside the sphere or there is just one intersection
    if(x02 >= FixedPoint.One) {
      // The disk rotates the circle if it strictly contains one point on the circle
      // We test the two points that are furthest / closest relative to the disk center, since one could be tangent
      // First, take the convex angle between the circle and disk centers
      val angle = x1.convexAngle(x2)
      val diskContainsCircle = {
        // Compute the first distance from the disk center by subtracting the radius of the circle
        // angle.radians and circle.radius.radians are in the range (0,pi), so the difference is in the range (-pi,pi)
        val dist = angle.radians - circle.radius.radians
        // If the absolute value is less than the disk radius, then the disk contains this point
        dist.abs < disk.radius.radians
      } || {
        // Compute the second distance from the disk center by adding the radius of the circle
        // angle.radians and circle.radius.radians are in the range (0,pi), so the sum is in the range (0,2*pi)
        val dist = angle.radians + circle.radius.radians
        // The distance from 0 or 2*pi must be less than the disk radius
        dist < disk.radius.radians || dist > FixedPoint.TwoPi - disk.radius.radians
      }
      // If the disk contains the circle, it contains all of this
      if(diskContainsCircle) this
      // Otherwise, return empty arc
      else new ArcsOnCircle(circle, zero, UnitArcs.Empty)
    }
    // If |x0| < 1, there are two intersections
    else {
      // The two intersections differ from x0 by some scalar multiple of n, the cross product of x1 and x2
      val n = x1.crossProduct(x2)
      // The scaling factor on n is +-t
      val t = FixedPoint.sqrt((FixedPoint.One - x02) / n.normSquared)
      // Product of n by scalar multiple t
      val nt = n * t

      // Using right hand rule, the start point of the intersection arc relative to the circle's center is x0 - nt
      val startPoint = new Point(x0 - nt)
      // Likewise, the end point relative to the circle's center is x0 + nt
      val endPoint = new Point(x0 + nt)
      // Take these points and convert them to an arc relative to the zero point
      val intersectionArc = UnitArcs(center.angle(zero, startPoint).radians, center.angle(zero, endPoint).radians)
      // Take the intersection with unitArcs
      new ArcsOnCircle(circle, zero, unitArcs.intersection(intersectionArc))
    }
  }

  /**
    * Convert arcs about an equivalent circle to equivalent arcs in the same of reference as `this`.
    * @param that Arcs about an equivalent circle (i.e. requires `circle == that.circle`).
    * @return `UnitArcs` around `that`, but with respect to the center and zero point of `this`.
    */
  def arcsAroundThis(that: ArcsOnCircle): UnitArcs = {
    val thoseArcsUnrotated =
      // Circles have same center, so counterclockwise direction is the same
      if(center == that.center) that.unitArcs
      // Circles have opposite centers, so counterclockwise direction is opposite; need to mirror
      else that.unitArcs.mirror
    // Angle that the arcs need to be rotated around this.center so they start at this.zero instead of that.zero
    // This is the same as the angle between this.zero and that.zero with respect to this.center
    // For example, if that.zero is 1 radian from this.zero, arcs that start at that.zero start at this.zero + 1 radian
    // So, thoseArcsUnRotated would have to be rotated by 1 radian to be in the frame of reference of this.zero
    val angle = center.angle(zero, that.zero).radians
    // Rotate by the angle
    thoseArcsUnrotated.rotate(angle)
  }

  override def sameCircleUnion(that: ArcsOnCircle): ArcsOnCircle = {
    // Convert to arcs around the same frame of reference
    val thatArcs = arcsAroundThis(that)
    // Use this frame of reference and the union of the arcs
    new ArcsOnCircle(circle, zero, unitArcs.union(thatArcs))
  }

  override def sameCircleDifference(that: ArcsOnCircle): ArcsOnCircle = {
    // Convert to arcs around the same frame of reference
    val thatArcs = arcsAroundThis(that)
    // Use this frame of reference and the difference of the arcs
    new ArcsOnCircle(circle, zero, unitArcs.difference(thatArcs))
  }

  override def nonEmpty: Boolean = {
    unitArcs.nonEmpty
  }
}
