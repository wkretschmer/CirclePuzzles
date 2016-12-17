package circlepuzzles.puzzle

import circlepuzzles.math._

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Parts on circle puzzles with boundaries that form nonintersecting continuous loops in the plane. Parts do not have
  * orientation, so two parts with the same boundary are considered equal.
  * @param boundary Nonempty list of all arcs in the boundary of this part. This list must be ordered, in the sense that
  * adjacent arcs in the list must share an endpoint. The list must also loop around (i.e the last arc must share an
  * endpoint with the first arc). However, this list does not need to be simplified. For example, there can be adjacent
  * arcs in the list around the same circle.
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
    * Tests if the given circle rotates this part. This may return an incorrect answer if the part was not cut by the
    * given circle.
    * @param circle A circle that specifies a center of rotation and radius.
    * @return True if and only if the given circle rotates this part.
    */
  def rotatedBy(circle: Circle): Boolean = {
    // If the boundary of the part has at least 3 endpoints, then they can't all belong to the boundary of the circle
    // because then they would be combined. In that case, assuming this part was split by the circle, this part must
    // have one endpoint strictly contained in the circle.
    if(boundarySet.size >= 3) {
      boundarySet.exists{arc =>
        // Have to check both endpoints because two arcs could start at the same point
        circle.strictlyContains(arc.startPoint) || circle.strictlyContains(arc.endPoint)
      }
    }
    // Otherwise, it is possible that the boundary set contains just two arcs, in which case both endpoints could be on
    // the boundary of this circle. We get around this by looking at the midpoints of the two arcs.
    else if(boundarySet.size == 2) {
      boundarySet.exists{arc =>
        val middleAngle = (arc.start + arc.end) / FixedPoint.Two
        // We add pi to get the actual mid angle if the arc wraps around zero
        val actualMidAngle =
          if(arc.end > arc.start) middleAngle
          else FixedPoint.mod2Pi(middleAngle + FixedPoint.Pi)
        val x = arc.circle.center.x + arc.circle.radius * FixedPoint.cos(actualMidAngle)
        val y = arc.circle.center.y + arc.circle.radius * FixedPoint.sin(actualMidAngle)
        circle.strictlyContains(FixedPoint2D(x, y))
      }
    }
    // Otherwise, the boundary set contains just one arc. We just test that the other circle contains the boundary arc.
    else {
      val arc = boundarySet.head
      // If the arc isn't a full circle, something has gone horribly wrong
      if(arc.start != arc.end) throw new IllegalArgumentException("Part boundary not closed: " + arc)
      // The arc must have smaller radius to be contained in the circle. Then, because we assume the arc was cut by the
      // circle, it suffices to just check that the circle contains its center.
      arc.circle.radius < circle.radius && circle.strictlyContains(arc.circle.center)
    }
  }

  /**
    * Computes the image of this part under the given move. This may return an incorrect answer if the part was not cut
    * by the move's circle.
    * @param move Move by which to rotate.
    * @return The image of this part under rotation by the given move, or `this` if the move does not rotate this part.
    */
  def image(move: Move): Part = {
    if(rotatedBy(move.circle)) {
      val rotatedBoundary = simplifiedBoundary.map{arc =>
        val rotatedCircle = arc.circle.rotate(move.circle.center, move.angle)
        val rotatedStart = FixedPoint.mod2Pi(arc.start + move.angle)
        val rotatedEnd = FixedPoint.mod2Pi(arc.end + move.angle)
        Arc(rotatedCircle, rotatedStart, rotatedEnd)
      }
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
