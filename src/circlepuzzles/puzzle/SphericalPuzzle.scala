package circlepuzzles.puzzle

import java.util

import circlepuzzles.geometry.{Angle, HasSphericalGeometry}

/**
  * Circle puzzles on the unit sphere.
  */
object SphericalPuzzle extends GeometricPuzzle with HasSphericalGeometry {
  import geom._

  override def emptySortedArcs(point: Point): util.TreeSet[(Arc, Boolean)] = ???

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
