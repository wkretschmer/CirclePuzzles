package circlepuzzles.puzzle

import circlepuzzles.math._

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Parts on circle puzzles with boundaries that form nonintersecting continuous loops in the plane.
  * @param boundary All arcs in the boundary of this part. This list must be ordered, in the sense that adjacent arcs in
  * the list must share an endpoint. The list must also loop around (i.e the last arc must share an endpoint with the
  * first arc). However, this list does not need to be simplified. For example, there can be adjacent arcs in the list
  * around the same circle.
  */
class Part(boundary: List[Arc]) {
  /**
    * Computes a simplified boundary; see above for a definition of simplified.
    * @param arcs Arcs to simplify.
    * @param current List buffer containing the arcs that have been simplified so far. Defaults to the empty list.
    * @return A simplified list of arcs.
    */
  @tailrec
  private def simplify(arcs: List[Arc], current: mutable.ListBuffer[Arc] = mutable.ListBuffer[Arc]()): List[Arc] = {
    arcs match {
      // See if we can combine these two arcs
      case first :: second :: rest =>
        // They can be combined if they belong to the same circle and share and endpoint
        if(first.circle == second.circle) {
          // Combine the first two arcs, but don't add to the buffer yet because they might be combinable with more arcs
          if(first.end == second.start) simplify(Arc(first.circle, first.start, second.end) :: rest, current)
          else if(first.start == second.end) simplify(Arc(first.circle, second.start, first.end) :: rest, current)
          else throw new IllegalArgumentException("Adjacent arcs don't touch: " + first + ", " + second)
        }
        // Otherwise, add the first arc and process the remaining arcs
        else {
          current.append(first)
          simplify(arcs.tail, current)
        }
      // See if we can combine the last and first arcs
      case last :: Nil =>
        // They can be combined if they belong to the same circle and share and endpoint
        if(current.nonEmpty && last.circle == current.head.circle) {
          val head = current.head
          // Need to remove the first arc from the list buffer, which is why we take the tail
          if(last.end == head.start) Arc(last.circle, last.start, head.end) :: current.toList.tail
          else if(last.start == head.end) Arc(last.circle, head.start, last.end) :: current.toList.tail
          else throw new IllegalArgumentException("Adjacent arcs don't touch: " + head + ", " + last)
        }
        // Otherwise, add the last arc and return the list
        else {
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
  val simplifiedBoundary = simplify(boundary)

  /**
    * The set of arcs in the simplified boundary. Two parts are equal if and only if this field is equal.
    */
  val boundarySet = simplifiedBoundary.toSet

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
    * Returns a hash code that satisfies the [[Object]] contract, which is to say that the hash depends only on
    * `boundarySet`.
    * @return A hash code depending only on `boundarySet`.
    */
  override def hashCode = boundarySet.hashCode()
}
