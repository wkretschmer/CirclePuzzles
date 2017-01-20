package circlepuzzles.puzzle

import java.util

import circlepuzzles.geometry.HasSphericalGeometry

/**
  * Circle puzzles on the unit sphere.
  */
object SphericalPuzzle extends GeometricPuzzle with HasSphericalGeometry {
  import geom._

  override def emptySortedArcs(point: Point): util.TreeSet[(Arc, Boolean)] = ???

  override def flatten(grouped: Iterable[ArcsOnCircle]): Iterable[Arc] = ???
}
