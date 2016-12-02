package circlepuzzles.math

/**
  * Immutable single arcs on the plane. This represents a nonempty closed arc that spans from `start` to `end` in the
  * counterclockwise direction.
  *
  * If `start < end`, the arc has angle `end - start`. If `start == end`, represents a full circle. If `start > end`,
  * the arc has angle `2*pi - (start - end).`
  * @param circle Circle to which this arc belongs.
  * @param start Angle at which this arc begins. Must be in the range [0,2*pi).
  * @param end Angle at which this arc ends. Must be in the range [0,2*pi).
  */
case class Arc(circle: Circle, start: FixedPoint, end: FixedPoint) {
  /**
    * The (x, y) coordinates of this circle's start point.
    */
  val startPoint = {
    val x = circle.x + circle.radius * FixedPoint.cos(start)
    val y = circle.y + circle.radius * FixedPoint.sin(start)
    (x, y)
  }

  /**
    * The (x, y) coordinates of this circle's end point.
    */
  val endPoint = {
    val x = circle.x + circle.radius * FixedPoint.cos(end)
    val y = circle.y + circle.radius * FixedPoint.sin(end)
    (x, y)
  }
}
