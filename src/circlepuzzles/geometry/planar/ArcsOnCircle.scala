package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{Angle, PlanarGeometry}
import circlepuzzles.math.FixedPoint._
import circlepuzzles.math.UnitArcs

/**
  * Collections of arcs around a circle in the Euclidean plane.
  * @param circle Circle to which these arcs belong.
  * @param unitArcs Arcs that belong to this collection, measured as angles in the counterclockwise direction relative to
  * the positive x-axis.
  */
class ArcsOnCircle(override val circle: Circle, val unitArcs: UnitArcs) extends PlanarGeometry.BaseArcsOnCircle {
  override def rotate(rotationCenter: Point, angle: Angle): ArcsOnCircle = {
    // Rotate both the circle and the unitArcs
    new ArcsOnCircle(circle.rotate(rotationCenter, angle), unitArcs.rotate(angle.radians))
  }

  override def sameCircleUnion(that: ArcsOnCircle): ArcsOnCircle = {
    // Compute the union of the arcs; circle == that.circle is unchecked
    new ArcsOnCircle(circle, unitArcs.union(that.unitArcs))
  }

  override def sameCircleDifference(that: ArcsOnCircle): ArcsOnCircle = {
    // Compute the difference of the arcs; circle == that.circle is unchecked
    new ArcsOnCircle(circle, unitArcs.difference(that.unitArcs))
  }

  override def intersection(disk: Disk): ArcsOnCircle = {
    // Distances between x and y coordinates
    val dx = disk.center.x - center.x
    val dy = disk.center.y - center.y
    // Radii of circles
    val thisRadius = circle.radius
    val diskRadius = disk.circle.radius
    // Distance between centers
    val distSquared = dx.pow(2) + dy.pow(2)
    val dist = sqrt(distSquared)
    if(dist >= thisRadius + diskRadius) { // Circles don't intersect or are tangent at one point
      new ArcsOnCircle(circle, UnitArcs.Empty)
    }
    else if(dist <= (thisRadius - diskRadius).abs) { // Circle of smaller radius contained in the other
      if(thisRadius < diskRadius) { // this contained in that
        new ArcsOnCircle(circle, UnitArcs.FullCircle)
      }
      else { // that contained in this, or circles are equal
        new ArcsOnCircle(circle, UnitArcs.Empty)
      }
    }
    else {
      val thisRadiusSquared = thisRadius.pow(2)
      // See http://paulbourke.net/geometry/circlesphere/
      val a = (distSquared + thisRadiusSquared - diskRadius.pow(2)) / (Two * dist)
      val h = sqrt(thisRadiusSquared - a.pow(2))
      // (midX, midY) is the intersection of the line between the centers and the line between the circle intersections
      val midX = center.x + a * dx / dist
      val midY = center.y + a * dy / dist
      // First intersection
      val intersection1X = midX + h * dy / dist
      val intersection1Y = midY - h * dx / dist
      val angle1 = atan2Mod2Pi(intersection1Y - center.y, intersection1X - center.x)
      // Second intersection
      val intersection2X = midX - h * dy / dist
      val intersection2Y = midY + h * dx / dist
      val angle2 = atan2Mod2Pi(intersection2Y - center.y, intersection2X - center.x)
      // It is not at all obvious that the arc necessarily starts at angle1 and ends at angle2. One can verify it by
      // considering the possible signs of dx and dy.
      new ArcsOnCircle(circle, UnitArcs(angle1, angle2))
    }
  }

  override def nonEmpty: Boolean = {
    unitArcs.nonEmpty
  }
}
