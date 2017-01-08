package circlepuzzles.puzzle

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Parts belonging to a puzzle in a particular geometry.
  */
trait GeometricPart extends GeometricMove {
  import geom._

  /**
    * Parts on circle puzzles with boundaries that form nonintersecting continuous loops. Parts do not have orientation,
    * so two parts with the same boundary are considered equal.
    * @param boundary Nonempty list of all arcs in the boundary of this part. This list must be ordered, in the sense
    * that adjacent arcs in the list must share an endpoint. The list must also loop around (i.e the last arc must share
    * an endpoint with the first arc). However, this list does not need to be simplified; there can be adjacent arcs in
    * the list around the same circle.
    */
  class Part(boundary: List[Arc]) {
    /**
      * Computes a simplified boundary. This is a geometrically equivalent boundary such that no two adjacent arcs in
      * the list belong to the same circle and overlap.
      * @param arcs Arcs to simplify.
      * @param current List buffer containing the arcs that have been simplified so far. Defaults to the empty list.
      * @return A simplified list of arcs.
      */
    @tailrec
    private def simplify(arcs: List[Arc], current: mutable.ListBuffer[Arc] = mutable.ListBuffer[Arc]()): List[Arc] = {
      arcs match {
        case first :: second :: rest =>
          // See if we can combine the first and second arcs
          first.union(second) match {
            case Some(combined) =>
              // Use combined arcs, but don't add to the buffer yet because they might be combinable with more arcs
              simplify(combined :: rest, current)
            case None =>
              // Otherwise, add the first arc and recurse on the remaining arcs
              current.append(first)
              simplify(arcs.tail, current)
          }
        case last :: Nil =>
          // See if we can combine the first and last arcs
          current.head.union(last) match {
            case Some(combined) =>
              // Call tail on current because we shouldn't include the first arc
              combined :: current.toList.tail
            case None =>
              // Otherwise, append the last arc normally and return the list
              current.append(last)
              current.toList
          }
        case _ =>
          current.toList
      }
    }

    /**
      * The simplified boundary of this part, wherein adjacent arcs around the same circle are combined.
      */
    val simplifiedBoundary: List[Arc] = simplify(boundary)

    /**
      * The set of arcs in the simplified boundary. Two parts are equal if and only if this field is equal.
      */
    val boundarySet: Set[Arc] = simplifiedBoundary.toSet

    /**
      * Tests if the given disk rotates this part. This may return an incorrect answer if the part was not cut by the
      * given disk.
      * @param disk Disk of rotation.
      * @return True if and only if the given disk rotates this part.
      */
    def rotatedBy(disk: Disk): Boolean = {
      // If the boundary of the part has at least 3 endpoints, then they can't all belong to the boundary of the disk
      // because then they would be combined. In that case, assuming this part was split by the disk, this part must
      // have one endpoint strictly contained in the disk.
      if(boundarySet.size >= 3) {
        boundarySet.exists{arc =>
          // Have to check both endpoints because two arcs could start at the same point
          disk.strictlyContains(arc.startPoint) || disk.strictlyContains(arc.endPoint)
        }
      }
      // Otherwise, it is possible that the boundary set contains just two arcs, in which case both endpoints could be
      // on the boundary of this disk. We get around this by looking at the midpoints of the two arcs.
      else if(boundarySet.size == 2) {
        boundarySet.exists{arc =>
          disk.strictlyContains(arc.midPoint)
        }
      }
      // Otherwise, the boundary set contains just one arc. We just test that the disk contains the boundary arc.
      // Assuming the part has been cut by the disk, it suffices to check that any point on the arc is in the disk's
      // interior. We choose the arc's start point.
      else {
        boundarySet.exists{arc =>
          disk.strictlyContains(arc.startPoint)
        }
      }
    }

    /**
      * Computes the image of this part under the given move. This may return an incorrect answer if the part was not
      * cut by the move's disk.
      * @param move Move by which to rotate.
      * @return Image of this part under rotation by the given move, or `this` if the move does not rotate this part.
      */
    def image(move: Move): Part = {
      if(rotatedBy(move.disk)) {
        val rotatedBoundary = simplifiedBoundary.map(arc => arc.rotate(move.disk.center, move.angle))
        new Part(rotatedBoundary)
      }
      else this
    }

    /**
      * Tests if the other object is an equal `Part`.
      * @param that Object to compare to.
      * @return `true` if and only if `that` is a `Part` with an equal boundary set.
      */
    override def equals(that: Any): Boolean = {
      that match {
        case thatPart: Part =>
          boundarySet == thatPart.boundarySet
        case _ =>
          false
      }
    }

    /**
      * A hash code that satisfies the [[Object]] contract, which is to say that the hash depends only on `boundarySet`.
      * Memoized.
      */
    override val hashCode = boundarySet.hashCode()
  }
}
