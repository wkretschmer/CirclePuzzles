package circlepuzzles.geometry.planar

import circlepuzzles.geometry.{Angle, PlanarGeometry}
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

  override def intersection(disk: Disk): ArcsOnCircle = ???

  override def nonEmpty: Boolean = {
    unitArcs.nonEmpty
  }
}
