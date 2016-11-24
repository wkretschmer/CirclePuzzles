package circlepuzzles.math

/**
  * Immutable sets of closed, disjoint arcs around the unit circle.
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
  * Such a list is called "simplified". All methods that return UnitArcs will simplify their results.
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
    * @param angle Counterclockwise rotation angle in radians. Requires `0 <= angle < 2*pi`.
    * @return `UnitArcs` resulting from a rotation by the angle specified.
    */
  def rotate(angle: FixedPoint): UnitArcs = {
    val rotated = arcs.map{case (f, p) => (f + angle, p)}
    // Arcs that start after 2*pi need to wrap around
    val (start, end) = rotated.span{case (f, p) => f < FixedPoint.TwoPi}
    val wrapped = end.map{case (f, p) => (f - FixedPoint.TwoPi, p)}
    val combined = wrapped ::: start
    // An arc list must start with zero
    val result = if(combined.head._1 == FixedPoint.Zero) combined else (FixedPoint.Zero, start.last._2) :: combined
    new UnitArcs(simplify(result))
  }

  /**
    * Helper method for `unionAndDifference`; see that method for a complete description.
    *
    * Note that the boolean parameters are effectively ignored when both lists are nonempty, and the first entries in
    * both lists have `FixedPoint` value 0. Thus, it makes sense to call `computeUnionDifference(minuend, subtrahend)`
    * on two arc lists that both start with 0, for example.
    * @param minuend First list to combine. This is the minuend for the difference computation.
    * @param subtrahend Second list to combine. This is the subtrahend for the difference computation.
    * @param minPresent Whether the interval immediately preceding `minuend.first` is present. Defaults to false.
    * @param subPresent Whether the interval immediately preceding `subtrahend.first` is present. Defaults to false.
    * @return Pair of arc lists containing the set of arcs in either `minuend` or `subtrahend`, and the set of arcs in
    * `subtrahend` but not `minuend`. Neither list is simplified.
    */
  private def computeUnionDifference(minuend: ArcList, subtrahend: ArcList,
                                     minPresent: Boolean = false, subPresent: Boolean = false): (ArcList, ArcList) = {
    // A mergesort-like procedure
    // Here, "F" and "P" refer respectively to the FixedPoint and Boolean in a given ArcList entry
    (minuend, subtrahend) match {
      // Both lists nonempty
      case ((nextMinF, nextMinP) :: restMin, (nextSubF, nextSubP) :: restSub) =>
        // Add the smaller endpoint, and continue the computation with the smaller endpoint removed from its
        // corresponding list (or from both lists if the endpoints are equal)
        if(nextMinF < nextSubF) {
          val (restUnion, restDifference) = computeUnionDifference(restMin, subtrahend, nextMinP, subPresent)
          val union = (nextMinF, nextMinP || subPresent) :: restUnion
          val difference = (nextMinF, !nextMinP && subPresent) :: restDifference
          (union, difference)
        }
        else if(nextSubF < nextMinF) {
          val (restUnion, restDifference) = computeUnionDifference(minuend, restSub, minPresent, nextSubP)
          val union = (nextSubF, minPresent || nextSubP) :: restUnion
          val difference = (nextSubF, !minPresent && nextSubP) :: restDifference
          (union, difference)
        }
        else {
          val (restUnion, restDifference) = computeUnionDifference(restMin, restSub, nextMinP, nextSubP)
          val union = (nextMinF, nextMinP || nextSubP) :: restUnion
          val difference = (nextMinF, !nextMinP && nextSubP) :: restDifference
          (union, difference)
        }
      // Both lists empty
      case (Nil, Nil) =>
        (Nil, Nil)
      // Minuend nonempty
      case (_, Nil) =>
        // Use the fact that presence of the second arc is known for the remainder of the interval (i.e. until 2*pi)
        val union = minuend.map{case (minF, minP) => (minF, minP || subPresent)}
        val difference = minuend.map{case (minF, minP) => (minF, !minP && subPresent)}
        (union, difference)
      // Subtrahend nonempty
      case (Nil, _) =>
        // Use the fact that presence of the first arc is known for the remainder of the interval (i.e. until 2*pi)
        val union = subtrahend.map{case (subF, subP) => (subF, minPresent || subP)}
        val difference = subtrahend.map{case (subF, subP) => (subF, !minPresent && subP)}
        (union, difference)
    }
  }

  /**
    * Compute the union and difference of this set of arcs with another set of arcs. The union consists of all arc
    * segments that belong to either set. The difference consists of all arc segments that belong to `this` but not
    * `that`.
    *
    * In both cases, the closure is returned. For example, if `this` contains a segment `[a,b]` and `that` contains a
    * segment `[b,c]`, the union will contain `[a,c]`.
    * @param that Arc set with which to combine.
    * @return A pair (union, difference) of arc sets as described above.
    */
  def unionAndDifference(that: UnitArcs): (UnitArcs, UnitArcs) = {
    val (union, difference) = computeUnionDifference(arcs, that.arcs)
    (new UnitArcs(simplify(union)), new UnitArcs(simplify(difference)))
  }
}
