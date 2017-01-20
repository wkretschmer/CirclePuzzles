package circlepuzzles.geometry.spherical

import circlepuzzles.geometry.{Angle, SphericalGeometry}
import circlepuzzles.math.UnitArcs

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

  override def intersection(disk: Disk): ArcsOnCircle = ???

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
    // This is the same as the angle between that.zero and this.zero with respect to this.center
    val angle = center.angle(that.zero, zero).radians
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
