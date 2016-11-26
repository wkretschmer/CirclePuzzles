package circlepuzzles.puzzle

import circlepuzzles.math._

import scala.collection.mutable

/**
  * Circle puzzles in the plane. A puzzle is defined by its atomic moves. Each move rotates the interior of its circle
  * by a fixed fraction of 2*pi; this action determines the possible states of the puzzle.
  * @param moves Set of allowed moves that generate this puzzle.
  */
class Puzzle(val moves: Set[Move]) {
  /**
    * A set of cuts. An entry `(circle, arcs)` such a map indicates that the arcs in `arcs` around the circle `circle`
    * belong in the cut set.
    */
  type CutSet = mutable.Map[Circle, UnitArcs]

  /**
    * Add the cuts specified by the given `Circle` and `UnitArcs` to the given `CutSet`. This works by setting
    * `cuts(circle)` equal to the union of `cuts(circle)` and `arcs`.
    * @param circle `Circle` to which the added cuts belong.
    * @param arcs `UnitArcs` describing the cuts around the given circle.
    * @param cuts `CutSet` to which the cuts are to be added.
    * @return `UnitArcs` that previously existed in the `CutSet` at the specified `Circle`.
    */
  private def add(circle: Circle, arcs: UnitArcs, cuts: CutSet): UnitArcs = {
    cuts.get(circle) match {
      case None =>
        cuts.put(circle, arcs)
        UnitArcs.Empty
      case Some(existingArcs) =>
        cuts.put(circle, arcs.union(existingArcs))
        existingArcs
    }
  }

  /**
    * Computes the set of cuts that split pieces in this puzzle. Equivalently, this computes all of the possible images
    * of the move `Circle`s under the action of rotation by `Move`s in `moves`.
    *
    * This may not terminate for infinite puzzles (i.e. puzzles that jumble).
    * @return The set of all cuts that exist in a full unbandaging of this puzzle.
    */
  def cutSet: CutSet = {
    val movesList = moves.toList
    // Start with one complete cut for each move's circle
    val allCuts = mutable.Map(movesList.map(move => (move.circle, UnitArcs.FullCircle)):_*)
    // For each move, maintain a set of cuts whose images have not been computed under that move
    // Initialized to all cuts (except for the cut corresponding to the move itself)
    val toProcess = movesList.map(move => (move, allCuts.filterNot(_._1 == move.circle))).toMap
    // Repeat while there are cuts whose images have not been computed
    while(toProcess.values.exists(_.nonEmpty)) {
      // Iterate over one move (and its set myCuts of cuts to process) at a time
      for((move, myCuts) <- toProcess ; (circle, arcs) <- myCuts) {
        // Process one set of concentric, coradial cuts at a time
        for((circle, arcs) <- myCuts) {
          // First verify that the cuts actually intersect nontrivially
          val intersection = circle.intersectionArc(move.circle).intersection(arcs)
          if(intersection.nonEmpty) {
            // Compute the image of the cuts under each repetition of this move
            for(angle <- move.nonzeroAngles) {
              // Rotate the cuts by rotating the center and arcs by the angle
              val rotatedCircle = circle.rotate(move.circle.x, move.circle.y, angle)
              val rotatedArcs = intersection.rotate(angle)
              // Add the rotated arcs, and get any concentric, coradial arcs that were already computed
              val existingArcs = add(rotatedCircle, rotatedArcs, allCuts)
              // Compute the newly added arcs by subtracting the arcs we computed previously
              val newArcs = rotatedArcs.difference(existingArcs)
              if(newArcs.nonEmpty) {
                // If any arcs were added, we need to compute their images under all other moves
                for((otherMove, otherCuts) <- toProcess if otherMove != move) {
                  // Add the new arcs to the toProcess set of the other move
                  add(rotatedCircle, newArcs, otherCuts)
                }
              }
            }
          }
        }
        // Having processed all of the cuts for a single move, clear its set in toProcess
        myCuts.clear()
      }
    }
    allCuts
  }
}
