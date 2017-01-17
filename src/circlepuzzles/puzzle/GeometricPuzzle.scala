package circlepuzzles.puzzle

import java.util

import scala.collection.mutable

/**
  * Circle puzzles in a particular geometry.
  */
trait GeometricPuzzle extends GeometricPart {
  import geom._

  /**
    * Turn an iterable of arcs grouped around particular circles into individual arcs. Arcs should be split at any
    * intersections with other arcs.
    * @param grouped Iterable of arcs grouped by circle.
    * @return A list of individual arcs such that any intersection between two arcs is an endpoint of both arcs.
    */
  def flatten(grouped: Iterable[ArcsOnCircle]): Iterable[Arc]

  /**
    * Produce an empty sorted set for arcs that all start or end at a single point. The boolean flag in such an entry
    * is true if the arc starts at this point, and false if it ends at this point. This set is sorted in a clockwise
    * direction, with the goal of making adjacent arcs in the set belong to individual parts.
    * @return An empty sorted set that compares arcs sharing a start or end point.
    */
  def emptySortedArcs(): util.TreeSet[(Arc, Boolean)]

  /**
    * Circle puzzles. A puzzle is defined by its atomic moves. Each move rotates its disk's interior by a fixed fraction
    * of 2*pi. This action determines the possible states of the puzzle.
    * @param moves Allowed moves that generate this puzzle.
    */
  class Puzzle(moves: Move*) {
    /**
      * A list of the moves that generate this puzzle.
      */
    val movesList = moves.distinct.toList

    /**
      * The set of cuts that split pieces in this puzzle. Equivalently, this is the set of all of the possible images of
      * the move `Circle`s under the action of rotation by `Move`s in `moves`.
      *
      * This may not terminate for infinite puzzles (i.e. puzzles that jumble).
      * @return All cuts in this puzzle, grouped by circle. Distinct entries in the returned iterable correspond to
      * different circles.
      */
    def groupedCuts: Iterable[ArcsOnCircle] = {
      // Start with one complete cut for each move's circle
      val allCuts = mutable.Map(movesList.map(move => (move.disk.circle, move.disk.circle.fullArcs)):_*)

      /**
        * Add the cuts specified by the given `ArcsOnCircle` to the given mutable cut set. This works by setting
        * `cuts(arcs.circle)` equal to the union of `cuts(arcs.circle)` and `arcs`.
        * @return `ArcsOnCircle` that previously existed in the mutable cut set around the same circle, or
        * `UnitArcs.Empty` if the key is new.
        */
      def add(arcs: ArcsOnCircle): ArcsOnCircle = {
        val circle = arcs.circle
        allCuts.get(circle) match {
          case None =>
            allCuts.put(circle, arcs)
            circle.emptyArcs
          case Some(existingArcs) =>
            allCuts.put(circle, existingArcs.sameCircleUnion(arcs))
            existingArcs
        }
      }

      // For each move, maintain a set of cuts whose images have not been computed under that move
      // Initialized to all cuts (except for the cut corresponding to the move itself)
      val toProcess = movesList.map(move => (move, allCuts.filterNot(_._1 == move.disk.circle))).toMap
      // Repeat while there are cuts whose images have not been computed
      while(toProcess.values.exists(_.nonEmpty)) {
        // Iterate over one move (and its set myCuts of cuts to process) at a time
        for((move, myCuts) <- toProcess) {
          // Process one set of concentric, coradial cuts at a time
          for(arcsOnCircle <- myCuts.values) {
            // First verify that the cuts actually intersect nontrivially
            val intersection = arcsOnCircle.intersection(move.disk)
            if(intersection.nonEmpty) {
              // Compute the image of the cuts under each repetition of this move
              for(angle <- move.nonzeroAngles) {
                // Rotate the cuts by rotating the center and arcs by the angle
                val rotatedArcs = intersection.rotate(move.disk, angle)
                // Add the rotated arcs, and get any concentric, coradial arcs that were already computed
                val existingArcs = add(rotatedArcs)
                // Compute the newly added arcs by subtracting the arcs we computed previously
                val newArcs = rotatedArcs.sameCircleDifference(existingArcs)
                if(newArcs.nonEmpty) {
                  // If any arcs were added, we need to compute their images under all other moves
                  for((otherMove, otherCuts) <- toProcess if otherMove != move) {
                    // Add the new arcs to the toProcess set of the other move
                    add(newArcs)
                  }
                }
              }
            }
          }
          // Having processed all of the cuts for a single move, clear its set in toProcess
          myCuts.clear()
        }
      }
      allCuts.values
    }

    /**
      * The set of cuts in this puzzle as an iterable over individual arcs that start and end at intersections with
      * other arcs. These are the arcs in a full unbandaging of this puzzle.
      * @return An iterable without duplicates of all arcs in a full unbandaging of this puzzle.
      */
    def flatCuts: Iterable[Arc] = {
      flatten(groupedCuts)
    }

    /**
      * The parts in this puzzle. The non-moving (possibly infinite) exterior is included as one of these parts, if
      * applicable.
      *
      * Note: this assumes that the boundary of all parts are nonintersecting continuous loops in the plane. This
      * assumption may be violated for puzzles that have disconnected moves, i.e. moves between which parts cannot be
      * exchanged. All other fields that depend on this may require the same assumption.
      */
    def parts: List[Part] = {
      // Map each arc intersection point to the set of arcs that start or end there. This is basically a graph where the
      // vertices are arc intersections and the edges are (Arc, Boolean) pairs where the boolean value is true if and
      // only if the arc starts at that vertex. Thus, a single TreeSet in the map is basically an adjacency set for the
      // corresponding point of intersection. The arcs around a single vertex are sorted in such a way that any two
      // adjacent sorted arcs belong to the same part. The ordering implicitly wraps around.
      // TODO could this be made immutable without hurting performance?
      val arcsByIntersection = mutable.Map[Point, util.TreeSet[(Arc, Boolean)]]()
      for(arc <- flatCuts) {
        // Make an empty sorted set of arcs for both intersections if they don't exist already, then add the arc to the
        // adjacency sets of both endpoints.
        arcsByIntersection.getOrElseUpdate(arc.startPoint, emptySortedArcs()).add((arc, true))
        arcsByIntersection.getOrElseUpdate(arc.endPoint, emptySortedArcs()).add((arc, false))
      }

      // Collect all parts in a single list
      var allParts = List[Part]()
      // Repeat while there exists a nonempty adjacency set, which implies there exists an edge that hasn't been added
      // to a part yet
      var next = arcsByIntersection.find(!_._2.isEmpty)
      while(next.nonEmpty) {
        val (startPoint, arcs) = next.get
        // It doesn't actually matter that we choose the first arc here
        val startArc = arcs.first()

        // Build up the part by collecting adjacent arcs that belong to its boundary until we loop back around to the
        // start vertex. This is where we use the fact that the adjacency sets are sorted.
        def makePartBoundary(currentArc: (Arc, Boolean)): List[Arc] = {
          val (arc, startedAtPrevious) = currentArc
          // Remove the arc from the previous adjacency set. Notice: we only remove the arc from one (not both) of its
          // endpoints. This is because each arc belongs to exactly two pieces, and we get the two pieces by traversing
          // once in each direction. The direction of traversal determines which piece the arc belongs to.
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
    def partIDs: Map[Part, Int] = parts.zipWithIndex.toMap

    /**
      * For each move, a list of `(move, map)` pairs where the map is from parts to parts, and represents the image
      * under that move of each part. Note that this ignores part orientation.
      */
    def partPermutations: List[(Move, Map[Part, Part])] = {
      movesList.map{move =>
        val permutation = parts.map{part =>
          (part, part.image(move))
        }.toMap
        (move, permutation)
      }
    }

    /**
      * Like `partPermutations`, but the maps are instead in terms of the integer IDs in `partIDs`.
      */
    def idPermutations: List[(Move, Map[Int, Int])] = {
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
    def permutationStrings: List[(Move, String)] = {
      for((move, idPermutation) <- idPermutations) yield {
        val list = List.tabulate(idPermutation.size)(i => idPermutation(i) + 1)
        (move, "[" + list.mkString(",") + "]")
      }
    }
  }

  /**
    * Like puzzle, but `groupedCuts` and all fields that depend on it are lazy vals. In other words, all fields are
    * memoized, but only computed on demand.
    * @param moves Allowed moves that generate this puzzle.
    */
  class LazyCachingPuzzle(moves: Move*) extends Puzzle(moves:_*) {
    override lazy val groupedCuts = super.groupedCuts
    override lazy val flatCuts = super.flatCuts
    override lazy val parts = super.parts
    override lazy val partIDs = super.partIDs
    override lazy val partPermutations = super.partPermutations
    override lazy val idPermutations = super.idPermutations
    override lazy val permutationStrings = super.permutationStrings
  }
}
