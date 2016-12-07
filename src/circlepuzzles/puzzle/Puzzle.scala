package circlepuzzles.puzzle

import java.util
import java.util.Comparator

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
  type CutSet = Map[Circle, UnitArcs]

  /**
    * Add the cuts specified by the given `Circle` and `UnitArcs` to the given mutable cut set. This works by setting
    * `cuts(circle)` equal to the union of `cuts(circle)` and `arcs`.
    * @param circle `Circle` to which the added cuts belong.
    * @param arcs `UnitArcs` describing the cuts around the given circle.
    * @param cuts Mutable cut set to which the cuts are to be added.
    * @return `UnitArcs` that previously existed in the mutable cut set at the specified `Circle`.
    */
  private def add(circle: Circle, arcs: UnitArcs, cuts: mutable.Map[Circle, UnitArcs]): UnitArcs = {
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
    * The set of cuts that split pieces in this puzzle. Equivalently, this is the set of all of the possible images of
    * the move `Circle`s under the action of rotation by `Move`s in `moves`.
    *
    * This may not terminate for infinite puzzles (i.e. puzzles that jumble).
    */
  lazy val cutSet: CutSet = {
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
    allCuts.toMap
  }

  /**
    * The set of cuts in this puzzle as an iterable over individual arcs that start and end at intersections with other
    * arcs. These are the arcs in a full unbandaging of this puzzle.
    */
  lazy val cutsAsArcs: Iterable[Arc] = {
    val cutsByCircle = for((circle, arcs) <- cutSet) yield {
      // Compute the set of possible intersections with other circles as angles around this circle
      val allIntersections = for((otherCircle, otherArcs) <- cutSet ;
                                 // Iterate through the 0, 1, or 2 intersections
                                 (angle, otherAngle) <- circle.intersections(otherCircle)
                                 // Only care about intersections that actually exist in the other circle
                                 if otherArcs.contains(otherAngle))
                             yield angle
      // Above, we don't explicitly check if the intersections are contained in arcs
      // This is because splitAtIntersections implicitly does this for us
      val concreteArcs = arcs.splitAtIntersections(allIntersections.toList.distinct.sorted)
      // Make an arc for each (start, end)
      for((start, end) <- concreteArcs) yield Arc(circle, start, end)
    }
    cutsByCircle.flatten
  }

  /**
    * The parts in this puzzle. The infinite exterior is included as one of these parts.
    *
    * Note: this assumes that the boundary of all parts are nonintersecting continuous loops in the plane. This
    * assumption may be violated for puzzles that have disconnected moves, i.e. moves between which parts cannot be
    * exchanged. All other fields that depend on this may require the same assumption.
    */
  lazy val parts: List[Part] = {
    // Map each arc intersection point to the set of arcs that start or end there. This is basically a graph where the
    // vertices are arc intersections and the edges are (Arc, Boolean) pairs where the boolean value is true if and only
    // if the arc starts at that vertex. Thus, a single TreeSet in the map is basically an adjacency list for the
    // corresponding point of intersection. The arcs around a single vertex are sorted in such a way that any two
    // adjacent sorted arcs belong to the same part. The ordering implicitly wraps around.
    val arcsByIntersection = mutable.Map[(FixedPoint, FixedPoint), util.TreeSet[(Arc, Boolean)]]()
    // Comparator for sorting arcs around a single intersection point
    val comparator = new Comparator[(Arc, Boolean)] {
      override def compare(a1: (Arc, Boolean), a2: (Arc, Boolean)): Int = {
        val (arc1, arc1StartsHere) = a1
        val (arc2, arc2StartsHere) = a2

        // First compute the angle of the line tangent to both arcs in the direction along the arc away from the
        // intersection point. Whether we add or subtract pi/2 depends on whether the arc starts or ends here.
        val arc1Angle = if(arc1StartsHere) FixedPoint.mod2Pi(arc1.start + FixedPoint.HalfPi)
                        else FixedPoint.mod2Pi(arc1.end - FixedPoint.HalfPi)
        val arc2Angle = if(arc2StartsHere) FixedPoint.mod2Pi(arc2.start + FixedPoint.HalfPi)
                        else FixedPoint.mod2Pi(arc2.end - FixedPoint.HalfPi)

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
    // Makes an empty sorted set with the comparator
    def makeSortedSet = new util.TreeSet[(Arc, Boolean)](comparator)
    for(arc <- cutsAsArcs) {
      // Make an empty sorted set of arcs for both intersections if they don't exist already, then add the arc to the
      // adjacency sets of both endpoints.
      arcsByIntersection.getOrElseUpdate(arc.startPoint, makeSortedSet).add((arc, true))
      arcsByIntersection.getOrElseUpdate(arc.endPoint, makeSortedSet).add((arc, false))
    }

    // Collect all parts in a single list
    var allParts = List[Part]()
    // Repeat while there exists a nonempty adjacency list, which implies there exists an edge that hasn't been added to
    // a part yet
    var next = arcsByIntersection.find(!_._2.isEmpty)
    while(next.nonEmpty) {
      val (startPoint, arcs) = next.get
      // It doesn't actually matter that we choose the first arc here
      val startArc = arcs.first()

      // Build up the part by collecting adjacent arcs that belong to its boundary until we loop back around to the
      // start vertex. This is where we use the fact that the adjacency lists are sorted.
      def makePartBoundary(currentArc: (Arc, Boolean)): List[Arc] = {
        val (arc, startedAtPrevious) = currentArc
        // Remove the arc from the previous adjacency list. Notice: we only remove the arc from one (not both) of its
        // endpoints. This is because each arc belongs to exactly two pieces, and we get the two pieces by traversing
        // once in each direction. The direction in which we traverse the arc determines which piece the arc belongs to.
        val previousPoint = if(startedAtPrevious) arc.startPoint else arc.endPoint
        arcsByIntersection(previousPoint).remove(currentArc)

        val nextPoint = if(startedAtPrevious) arc.endPoint else arc.startPoint
        // Stop if we returned to the start point of this part's boundary
        if(nextPoint == startPoint) List(arc)
        else {
          val nextPointArcs = arcsByIntersection(nextPoint)
          // This is where we loop around the ordering. If there is no next element, the next element is actually the
          // first element.
          val nextArc = Option(nextPointArcs.higher((arc, !startedAtPrevious))).getOrElse(nextPointArcs.first())
          arc :: makePartBoundary(nextArc)
        }
      }

      val boundary = makePartBoundary(startArc)
      allParts ::= new Part(boundary)
      next = arcsByIntersection.find(!_._2.isEmpty)
    }
    allParts
  }

  /**
    * Maps each part to a unique integer ID in the range `[0, parts.size)`.
    */
  lazy val partIDs: Map[Part, Int] = parts.zipWithIndex.toMap

  /**
    * For each move, a list of `(move, map)` pairs where the map is from parts to parts, and represents the image under
    * that move of each part.
    */
  lazy val partPermutations: List[(Move, Map[Part, Part])] = {
    moves.toList.map{move =>
      val permutation = parts.map{part =>
        (part, part.image(move))
      }.toMap
      (move, permutation)
    }
  }

  /**
    * Like `partPermutations`, but the maps are instead in terms of the integer IDs in `partIDs`.
    */
  lazy val idPermutations: List[(Move, Map[Int, Int])] = {
    for((move, permutation) <- partPermutations) yield {
      val idPermutation = for((part, image) <- permutation) yield {
        (partIDs(part), partIDs(image))
      }
      (move, idPermutation)
    }
  }

  /**
    * Like `partPermutations`, but the maps are turned into strings that GAP can interpret as a permutation using
    * `AsPermutation(Transformation(_))`. This also means that each ID is incremented by 1 to be in the range
    * `[1, parts.size]`.
    */
  lazy val permutationStrings: List[(Move, String)] = {
    for((move, idPermutation) <- idPermutations) yield {
      val list = List.tabulate(idPermutation.size)(i => idPermutation(i) + 1)
      (move, "[" + list.mkString(",") + "]")
    }
  }
}
