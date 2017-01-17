package circlepuzzles.puzzle

import java.util
import java.util.Comparator

import circlepuzzles.geometry.{Angle, HasPlanarGeometry}
import circlepuzzles.geometry.planar.Arc
import circlepuzzles.math.FixedPoint

object PlanarPuzzle extends GeometricPuzzle with HasPlanarGeometry {
  import geom._

  /**
    * Comparator for sorting arcs that start or end a single point.
    */
  val comparator = new Comparator[(Arc, Boolean)] {
    override def compare(a1: (Arc, Boolean), a2: (Arc, Boolean)): Int = {
      val (arc1, arc1StartsHere) = a1
      val (arc2, arc2StartsHere) = a2

      // First compute the angle of the line tangent to both arcs in the direction along the arc away from the
      // intersection point. Whether we add or subtract pi/2 depends on whether the arc starts or ends here.
      val arc1Angle = if(arc1StartsHere) FixedPoint.mod2Pi(arc1.start.radians + FixedPoint.HalfPi)
      else FixedPoint.mod2Pi(arc1.end.radians - FixedPoint.HalfPi)
      val arc2Angle = if(arc2StartsHere) FixedPoint.mod2Pi(arc2.start.radians + FixedPoint.HalfPi)
      else FixedPoint.mod2Pi(arc2.end.radians - FixedPoint.HalfPi)

      // If the angles are not the same, then order by whichever comes first in a counterclockwise direction
      val angleCompare = arc1Angle.compare(arc2Angle)
      if(angleCompare != 0) angleCompare
      // Otherwise, the arcs leave at the same angle, so we sort by some casework. This essentially sorts according to
      // the curvature of the arcs. It is more clear why this works if one draws a picture.
      else (arc1StartsHere, arc2StartsHere) match {
        // Among arcs that start here, the circle with smaller radius is considered larger
        case (true, true) => arc2.circle.radius.compare(arc1.circle.radius)
        // Among arcs that end here, the circle with greater radius is considered larger
        case (false, false) => arc1.circle.radius.compare(arc2.circle.radius)
        // Otherwise, the circle that starts here is considered larger
        case (true, false) => 1
        case (false, true) => -1
      }
    }
  }

  override def emptySortedArcs(point: Point): util.TreeSet[(Arc, Boolean)] = {
    // The comparator doesn't depend on the particular point we use
    new util.TreeSet[(Arc, Boolean)](comparator)
  }

  override def flatten(grouped: Iterable[ArcsOnCircle]): Iterable[Arc] = {
    val cutsByCircle = for(arcsOnCircle <- grouped) yield {
      val circle = arcsOnCircle.circle
      val unitArcs = arcsOnCircle.unitArcs
      // Compute the set of possible intersections with other circles as angles around this circle
      val allIntersections = for(otherArcsOnCircle <- grouped ;
                                 // Iterate through the 0, 1, or 2 intersections
                                 (angle, otherAngle) <- circle.intersections(otherArcsOnCircle.circle)
                                 // Only care about intersections that actually exist in the other circle
                                 if otherArcsOnCircle.unitArcs.contains(otherAngle))
                             yield angle
      // Above, we don't explicitly check if the intersections are contained in arcs
      // This is because splitAtIntersections implicitly does this for us
      val concreteArcs = unitArcs.splitAtIntersections(allIntersections.toList.distinct.sorted)
      // Make an arc for each (start, end)
      for((start, end) <- concreteArcs) yield Arc(circle, new Angle(start), new Angle(end))
    }
    cutsByCircle.flatten
  }
}
