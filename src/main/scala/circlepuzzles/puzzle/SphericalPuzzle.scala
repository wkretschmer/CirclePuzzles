package circlepuzzles.puzzle

import java.util
import java.util.Comparator

import circlepuzzles.geometry.{Angle, HasSphericalGeometry}
import circlepuzzles.math.FixedPoint

/**
  * Circle puzzles on the unit sphere.
  */
object SphericalPuzzle extends GeometricPuzzle with HasSphericalGeometry {
  import geom._

  override def emptySortedArcs(point: Point): util.TreeSet[(Arc, Boolean)] = {
    val zero = new Circle(point, new Angle(FixedPoint.HalfPi)).arbitraryPoint

    val comparator = new Comparator[(Arc, Boolean)] {
      override def compare(a1: (Arc, Boolean), a2: (Arc, Boolean)): Int = {
        val (arc1, arc1StartsHere) = a1
        val (arc2, arc2StartsHere) = a2

        // First compute the angle of the line tangent to both arcs in the direction along the arc away from the
        // intersection point. Whether we add or subtract pi/2 depends on whether the arc starts or ends here.
        val arc1Angle =
          if(arc1StartsHere) FixedPoint.mod2Pi(point.angle(zero, arc1.center).radians - FixedPoint.HalfPi)
          else FixedPoint.mod2Pi(point.angle(zero, arc1.center).radians + FixedPoint.HalfPi)
        val arc2Angle =
          if(arc2StartsHere) FixedPoint.mod2Pi(point.angle(zero, arc2.center).radians - FixedPoint.HalfPi)
          else FixedPoint.mod2Pi(point.angle(zero, arc2.center).radians + FixedPoint.HalfPi)

        // If the angles are not the same, then order by whichever comes first in a counterclockwise direction
        val angleCompare = arc1Angle.compare(arc2Angle)
        if(angleCompare != 0) angleCompare
        // Otherwise, the arcs leave at the same angle, so we sort by some casework. This essentially sorts according to
        // the curvature of the arcs. It is more clear why this works if one draws a picture.
        else (arc1StartsHere, arc2StartsHere) match {
          // Among arcs that start here, the circle with smaller radius is considered larger
          case (true, true) => arc2.circle.radius.radians.compare(arc1.circle.radius.radians)
          // Among arcs that end here, the circle with greater radius is considered larger
          case (false, false) => arc1.circle.radius.radians.compare(arc2.circle.radius.radians)
          // Otherwise, reduce to one of the previous cases by taking the supplement of one of the angles
          // This is equivalent to treating one of the arcs as its dual representation
          case (true, false) => arc2.circle.radius.radians.compare(arc1.circle.radius.supplement.radians)
          case (false, true) => arc1.circle.radius.radians.compare(arc2.circle.radius.supplement.radians)
        }
      }
    }

    new util.TreeSet[(Arc, Boolean)](comparator)
  }

  override def flatten(grouped: Iterable[ArcsOnCircle]): Iterable[Arc] = {
    val cutsByCircle = for(arcsOnCircle <- grouped) yield {
      val circle = arcsOnCircle.circle
      val zero = arcsOnCircle.zero
      val unitArcs = arcsOnCircle.unitArcs
      // Compute the set of possible intersections with other circles as angles around this circle
      val allIntersectionPoints = for(other <- grouped ;
                                 // Iterate through the 0, 1, or 2 intersections
                                 intersection <- circle.intersections(other.circle)
                                 // Only care about intersections that actually exist in the other circle
                                 if other.unitArcs.contains(other.center.angle(other.zero, intersection).radians))
        yield intersection
      val allIntersectionAngles = allIntersectionPoints.toList.distinct.map(circle.center.angle(zero, _).radians)
      // Above, we don't explicitly check if the intersections are contained in arcs
      // This is because splitAtIntersections implicitly does this for us
      val concreteArcs = unitArcs.splitAtIntersections(allIntersectionAngles.sorted)
      // Make an arc for each (start, end)
      for((start, end) <- concreteArcs) yield {
        val center = circle.center
        val startPoint = zero.rotate(center, new Angle(start))
        val endPoint = zero.rotate(center, new Angle(end))
        new Arc(center, startPoint, endPoint)
      }
    }
    cutsByCircle.flatten
  }
}
