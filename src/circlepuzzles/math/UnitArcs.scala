package circlepuzzles.math

/**
  * Immutable sets of closed, disjoint, positive length arcs around the unit circle. The arcs represented necessarily
  * form a perfect set, because the arcs are closed and have no isolated points.
  *
  * Note that operations like `union`, `intersection`, and `difference` maintain the perfect set property, so isolated
  * points in the set are ignored, and isolated points in the complement are part of the set.
  * @param arcs Underlying representation. An entry `(start, present)` that precedes an entry `(end, _)` in the list
  * indicates that this `UnitArcs` contains the points in the closed arc beginning at angle `start` and ending at angle
  * `end`, in radians. The only exception is for the last entry `(last, present)`, which indicates whether the arc from
  * angle `last` to 2*pi is in the list. The arcs represented are the closure of the union over individual present arcs
  * in the list.
  *
  * The list must be nonempty, and consist of `FixedPoints` in the half-closed interval [0,2*pi). The first entry in the
  * list must have `FixedPoint` value 0. Additionally, the list must be sorted strictly by `FixedPoint` value, which is
  * to say that it contains no duplicate `FixedPoint`s.
  *
  * A desirable (but not strictly necessary) property is for the list to alternate between present and non-present arcs.
  * Such a list is called "simplified". All methods that return `UnitArcs` will simplify their results.
  */
class UnitArcs(val arcs: List[(FixedPoint, Boolean)]) {
  /**
    * See above for a complete description of what an arc list represents. Note that in general, arc lists used in
    * intermediate computation do not need to start with 0; that an arc list starts with 0 is only required for arc
    * lists that represent `UnitArcs` instances.
    */
  type ArcList = List[(FixedPoint, Boolean)]

  /*
   * A note on performance:
   * The use of linked lists to store arcs means that most operations take time linear in the size of the list. This
   * choice of representation is based on an assumption that the number of arcs stored is never bigger than a small
   * constant.
   * Most of the operations on arc lists are written in a functional style. As a result, some operations are not fully
   * optimized, which is to say that running times could be reduced by a constant factor on those operations.
   * Nevertheless, the implementations here are unlikely to change unless they are found to be performance bottlenecks.
   */

  /**
    * Simplify the list by merging together adjacent arcs.
    * @param as List of arcs to simplify.
    * @return An equivalent list of arcs formed by joining runs of arcs that are all present or not present.
    */
  private def simplify(as: ArcList): ArcList = {
    as match {
      case (f1, p1) :: (f2, p2) :: rest =>
        // Both arcs either present or not present, so the second entry is unneeded
        if(p1 == p2) simplify((f1, p1) :: rest)
        // Otherwise, keep both arcs
        else (f1, p1) :: simplify(as.tail)
      case _ =>
        as
    }
  }

  /**
    * Rotate all contained arcs around the origin.
    * @param angle Counterclockwise rotation angle in radians.
    * @return `UnitArcs` resulting from a rotation by the angle specified.
    */
  def rotate(angle: FixedPoint): UnitArcs = {
    // Normalize to range [0,2*pi)
    val normalizedAngle = FixedPoint.mod2Pi(angle)
    val rotated = arcs.map{case (f, p) => (f + normalizedAngle, p)}
    // Arcs that begin after 2*pi need to wrap around
    // Thus, arcs in start don't wrap, and arcs in end do
    val (start, end) = rotated.span{case (f, p) => f < FixedPoint.TwoPi}
    val wrapped = end.map{case (f, p) => (f - FixedPoint.TwoPi, p)}
    val combined = wrapped ::: start
    // An arc list must start with zero
    val result = if(combined.head._1 == FixedPoint.Zero) combined else (FixedPoint.Zero, start.last._2) :: combined
    new UnitArcs(simplify(result))
  }

  /**
    * Helper method for computing union, intersection, difference, etc. of two arc lists.
    *
    * Note that the boolean parameters are effectively ignored when both lists are nonempty, and the first entries in
    * both lists have the same `FixedPoint` value. Thus, it makes sense to call `merge(keep, arcs1, arcs2)` on two arc
    * lists that both start with 0, for example.
    * @param keep A function that decides if an arc belongs in the merge given whether or not it was in the first and
    * second arc lists.
    * @param arcs1 First list to combine.
    * @param arcs2 Second list to combine.
    * @param arc1Present Whether the arc immediately preceding `arcs1.first` is present. Defaults to false.
    * @param arc2Present Whether the arc immediately preceding `arcs2.first` is present. Defaults to false.
    * @return An arc list containing the arcs for which `keep(p1, p2)` evaluates to `true`, where `p1` and `p2`
    * indicate whether the arc belongs to the first and second arc lists, respectively. The arc list is not simplified;
    * it contains all endpoints that belong to either list.
    */
  private def merge(keep: (Boolean, Boolean) => Boolean, arcs1: ArcList, arcs2: ArcList,
                    arc1Present: Boolean = false, arc2Present: Boolean = false): ArcList = {
    // A mergesort-like procedure
    // Here, "F" and "P" refer respectively to the FixedPoint and Boolean in a given ArcList entry
    (arcs1, arcs2) match {
      // Both lists nonempty
      case ((next1F, next1P) :: rest1, (next2F, next2P) :: rest2) =>
        // Add the smaller endpoint, and continue the computation with the smaller endpoint removed from its
        // corresponding list (or from both lists if the endpoints are equal)
        if(next1F < next2F) {
          (next1F, keep(next1P, arc2Present)) :: merge(keep, rest1, arcs2, next1P, arc2Present)
        }
        else if(next2F < next1F) {
          (next2F, keep(arc1Present, next2P)) :: merge(keep, arcs1, rest2, arc1Present, next2P)
        }
        else {
          (next1F, keep(next1P, next2P)) :: merge(keep, rest1, rest2, next1P, next2P)
        }
      // Both lists empty
      case (Nil, Nil) =>
        Nil
      // First list nonempty
      case (_, Nil) =>
        // Use the fact that presence of the second arc is known for the remainder of the interval (i.e. until 2*pi)
        arcs1.map{case (next1F, next1P) => (next1F, keep(next1P, arc2Present))}
      // Second list nonempty
      case (Nil, _) =>
        // Use the fact that presence of the first arc is known for the remainder of the interval (i.e. until 2*pi)
        arcs2.map{case (next2F, next2P) => (next2F, keep(arc1Present, next2P))}
    }
  }

  /**
    * Compute the difference with another set of arcs.
    * @param that Arc set with which to combine.
    * @return An arc set containing the points that exist in `this` but not `that`.
    */
  def difference(that: UnitArcs): UnitArcs = new UnitArcs(simplify(merge(_ && !_, arcs, that.arcs)))

  /**
    * Compute the intersection with another set of arcs.
    * @param that Arc set with which to combine.
    * @return An arc set containing the points that exist in both `this` and `that`.
    */
  def intersection(that: UnitArcs): UnitArcs = new UnitArcs(simplify(merge(_ && _, arcs, that.arcs)))

  /**
    * Compute the union with another set of arcs.
    * @param that Arc set with which to combine.
    * @return An arc set containing the points that exist in either `this` or `that`.
    */
  def union(that: UnitArcs): UnitArcs = new UnitArcs(simplify(merge(_ || _, arcs, that.arcs)))

  /**
    * Tests if this contains any arcs.
    * @return `true` if and only if `this` contains at least one arc.
    */
  def nonEmpty: Boolean = arcs.exists(_._2)

  /**
    * Tests if this contains the given angle.
    * @param angle An angle in the range [0,2*pi).
    * @return `true` if and only if the angle belongs to one of the arcs in this instance.
    */
  def contains(angle: FixedPoint): Boolean = {
    val (lessThan, greaterThanOrEqual) = arcs.span(_._1 < angle)
    // If angle is contained in arcs as an endpoint, then the angle is in the set if either adjacent arc is present
    // Note: this covers the case where angle is zero
    if(greaterThanOrEqual.nonEmpty && greaterThanOrEqual.head._1 == angle) {
      // lessThan is empty if and only if the angle is zero
      // In this case, the "previous" arc is actually the last arc in the list (i.e. we wrap around)
      greaterThanOrEqual.head._2 || lessThan.lastOption.getOrElse(arcs.last)._2
    }
    // Otherwise, the angle is positive and belongs to the last arc that starts before it
    // Because the angle is positive, lessThan is nonempty
    else lessThan.last._2
  }

  /**
    * Turn this abstract set of arcs into explicit `(start, end)` pairs of arcs that are contained in this instance.
    * Such a pair starts at `start` and sweeps in the counterclockwise direction until `end`. If `start == end`, this
    * indicates the presence of a complete circle.
    *
    * Arcs are also split at the specified list of points. For example, if this instance contains all points in the
    * range [1,3] and `List(2)` is given as input, the pairs `(1, 2)` and `(2, 3)` will be returned. If this `UnitArcs`
    * instance is not simplified, any extra endpoints will also be included as splitting points.
    * @param splits A sorted list of split angles without duplicates. All entries must be in the range [0,2*pi).
    * @return A list of `(start, end)` pairs representing arcs in this instance that have been split at the given
    * points. The list is not guaranteed to be sorted in any particular way.
    */
  def splitAtIntersections(splits: List[FixedPoint]): List[(FixedPoint, FixedPoint)] = {
    // Get split arc list using merge, which "unsimplifies" the list by introducing additional points that do nothing
    // Our keep function just ignores the second list
    val splitArcs = merge((fst, snd) => fst, arcs, splits.map(i => (i, false)))

    // Helper function for turning an arc list into explicit (start, end) arcs
    // It takes as input a tail of splitArcs (so by assumption, arcList does not include 0)
    // This function will also merge the last and first arcs if necessary
    def makeArcs(arcList: ArcList): List[(FixedPoint, FixedPoint)] = {
      arcList match {
        // Have two endpoints
        case (start, present) :: (end, _) :: rest =>
          // Add the arc between the endpoints if it is present
          if(present) (start, end) :: makeArcs(arcList.tail)
          // Otherwise process the rest of the list
          else makeArcs(arcList.tail)
        // Have just one endpoint; the other endpoint is the implicit wrap around to 0
        case (start, present) :: Nil =>
          // The last arc and the first arc merge if both are present, and 0 was not a split point
          if(present && splitArcs.head._2 && splits.head != FixedPoint.Zero) {
            // It is safe to call splitArcs(1) because the fact that we got here means splitArcs.tail.size >= 1
            List((start, splitArcs(1)._1))
          }
          // Otherwise, the two arcs remain separate
          else {
            // This is the least ugly way to produce a list of between zero and two elements depending on if the arcs
            // are present
            val lastArcOption = if(present) Some((start, FixedPoint.Zero)) else None
            val firstArcOption = if(splitArcs.head._2) Some((FixedPoint.Zero, splitArcs(1)._1)) else None
            List(lastArcOption, firstArcOption).flatten
          }
        // Notice that the only way we get Nil is if splitArcs.size == 1
        case Nil =>
          // We either have a full circle or no arcs
          if(splitArcs.head._2) List((FixedPoint.Zero, FixedPoint.Zero))
          else Nil
      }
    }

    makeArcs(splitArcs.tail)
  }
}

object UnitArcs {
  /**
    * A single arc about the whole unit circle.
    */
  val FullCircle = new UnitArcs(List((FixedPoint.Zero, true)))

  /**
    * An empty arc about the unit circle.
    */
  val Empty = new UnitArcs(List((FixedPoint.Zero, false)))

  /**
    * Creates an arc about the unit circle starting counterclockwise from the given start angle and ending at the given
    * end angle.
    * @param start Angle in radians at which this arc begins. Must be in the range [0,2*pi).
    * @param end Angle in radians at which this arc begins. Must be in the range [0,2*pi) and not equal to `start`.
    * @return An arc constructed counterclockwise from the specified start and end parameters.
    */
  def apply(start: FixedPoint, end: FixedPoint): UnitArcs = {
    // Arc doesn't wrap around zero
    if(end > start) {
      if(start > FixedPoint.Zero) new UnitArcs(List((FixedPoint.Zero, false), (start, true), (end, false)))
      else new UnitArcs(List((FixedPoint.Zero, true), (end, false)))
    }
    // Arc wraps around zero
    else {
      if(end > FixedPoint.Zero) new UnitArcs(List((FixedPoint.Zero, true), (end, false), (start, true)))
      else new UnitArcs(List((FixedPoint.Zero, false), (start, true)))
    }
  }
}
