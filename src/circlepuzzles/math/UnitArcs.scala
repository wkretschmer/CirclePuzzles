package circlepuzzles.math

/**
  * Immutable sets of closed, disjoint arcs around the unit circle.
  * @param intervals Underlying representation. An entry `(start, present)` that precedes an entry `(end, _)` in the
  * list indicates that this `UnitArcs` contains the points in the closed arc beginning at angle `start` and ending at
  * angle `end`. The only exception is for the last entry `(last, present)`, which indicates whether the arc from angle
  * `last` to 2*pi is in the list.
  *
  * The list must be nonempty, and consist of `FixedPoints` in the half-closed interval [0,2*pi). The first entry in the
  * list must have `FixedPoint` value 0. Additionally, the list must be sorted strictly by `FixedPoint` value, which is
  * to say that it contains no duplicate `FixedPoint`s.
  *
  * A desirable (but not strictly necessary) property is for the list to alternate between present and non-present arcs.
  * Such a list is called "simplified".
  */
class UnitArcs(val intervals: List[(FixedPoint, Boolean)]) {
  
}
