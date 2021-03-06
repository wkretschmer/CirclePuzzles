package circlepuzzles.geometry

/**
  * Objects that specify computations for a particular two-dimensional plane-like geometric setting. Subclasses of this
  * trait are generally singleton objects.
  *
  * The types `Point`, `Circle`, `Disk`, and `Arc` must correctly implement `equals` and `hashCode` methods according to
  * the [[Object]] contract. These types and `ArcsOnCircle` must all be immutable.
  */
trait Geometry {
  // Type members

  /**
    * Type of individual points.
    */
  type Point <: BasePoint

  /**
    * Type of circles. A circle is defined as a set of points that are equidistant from a center point.
    */
  type Circle <: BaseCircle

  /**
    * Type of disks in this geometry. A disk is defined as a closed set of points whose boundary is a circle. Depending
    * on context, this can either be the interior or exterior of a circle.
    */
  type Disk <: BaseDisk

  /**
    * Type of arcs. An arc is defined as a set of points on a circle between a given start and end point, inclusive.
    */
  type Arc <: BaseArc

  /**
    * Type of disjoint arc segments on a single circle.
    */
  type ArcsOnCircle <: BaseArcsOnCircle


  // Base traits for type members

  /**
    * Base trait for points. Immutable.
    */
  trait BasePoint {
    this: Point => // Every BasePoint must also be a Point

    /**
      * Rotate about the given point in the counterclockwise direction.
      * @param rotationCenter Center of rotation.
      * @param angle Angle of rotation.
      * @return Image of this under the specified rotation.
      */
    def rotate(rotationCenter: Point, angle: Angle): Point
  }

  /**
    * Base trait for circles. Immutable.
    */
  trait BaseCircle extends HasCenter {
    this: Circle => // Every BaseCircle must also be a Circle

    /**
      * Produces an empty set of arcs around this circle.
      * @return Empty set of arcs around this circle.
      */
    def emptyArcs: ArcsOnCircle

    /**
      * Produces a complete (i.e. full circle) set of arcs around this circle.
      * @return Full set of arcs around this circle.
      */
    def fullArcs: ArcsOnCircle

    /**
      * Rotate about the given point in the counterclockwise direction.
      * @param rotationCenter Center of rotation.
      * @param angle Angle of rotation.
      * @return Image of this under the specified rotation.
      */
    def rotate(rotationCenter: Point, angle: Angle): Circle
  }

  /**
    * Base trait for disks. Immutable.
    */
  trait BaseDisk extends HasCircle {
    this: Disk => // Every BaseDisk must also be a Disk

    /**
      * Return an integer indicating the location of the given point relative to this disk.
      * @param pt Point to test for membership.
      * @return A negative, zero, or positive integer if the point is respectively on the interior, boundary, or
      * exterior of this disk.
      */
    def containsCompare(pt: Point): Int

    /**
      * Test if the given point is in the interior or on the boundary of this disk.
      * @param pt Point to test for membership.
      * @return True if and only if this disk contains the given point.
      */
    def contains(pt: Point): Boolean = containsCompare(pt) <= 0

    /**
      * Test if the given point is in the interior of this disk.
      * @param pt Point to test for membership.
      * @return True if and only if this disk strictly contains the given point.
      */
    def strictlyContains(pt: Point): Boolean = containsCompare(pt) < 0

    /**
      * Rotate about the given point in the counterclockwise direction.
      * @param rotationCenter Center of rotation.
      * @param angle Angle of rotation.
      * @return Image of this under the specified rotation.
      */
    def rotate(rotationCenter: Point, angle: Angle): Disk
  }

  /**
    * Base trait for arcs. Immutable.
    */
  trait BaseArc extends HasCircle {
    this: Arc => // Every BaseArc must also be an Arc

    /**
      * Attempts to join two arcs that share an endpoint. If `this` and `that` belong to the same circle, this returns
      * `Some(joined)`, where `joined` is the arc formed by the connecting the arcs at their shared point. Otherwise,
      * returns `None`.
      * @param that Arc to combine that shares exactly one endpoint with this.
      * @return Joining of `this` and `that`, or `None` if they can't be combined.
      */
    def join(that: Arc): Option[Arc]

    /**
      * The point at which this arc begins, in the counterclockwise direction.
      * @return Start point of this arc.
      */
    def startPoint: Point

    /**
      * The point at which this arc ends, in the counterclockwise direction.
      * @return End point of this arc.
      */
    def endPoint: Point

    /**
      * The point on this arc which is equidistant from the start and end points.
      * @return Midpoint of this arc.
      */
    def midPoint: Point

    /**
      * Rotate about the given point in the counterclockwise direction.
      * @param rotationCenter Center of rotation.
      * @param angle Angle of rotation.
      * @return Image of this under the specified rotation.
      */
    def rotate(rotationCenter: Point, angle: Angle): Arc
  }

  /**
    * Base trait for arcs on a circle. Immutable.
    */
  trait BaseArcsOnCircle extends HasCircle {
    this: ArcsOnCircle => // Every BaseArcsOnCircle must also be an ArcsOnCircle

    /**
      * Compute the union of this with the given arcs by joining overlapping segments.
      * @param that Arcs around the same circle (i.e. requires `this.circle == that.circle`).
      * @return `ArcsOnCircle` containing all arcs that belong to either `this` or `that`.
      */
    def sameCircleUnion(that: ArcsOnCircle): ArcsOnCircle

    /**
      * Compute the difference of this with the given arcs by subtracting overlapping segments.
      * @param that Arcs around the same circle (i.e. requires `this.circle == that.circle`).
      * @return `ArcsOnCircle` containing all arcs that belong to `this` but not `that`.
      */
    def sameCircleDifference(that: ArcsOnCircle): ArcsOnCircle

    /**
      * Compute the arc segments in this collection that intersect with the given disk.
      * @param disk Disk with which to intersect.
      * @return The intersection of `this` with the given disk.
      */
    def intersection(disk: Disk): ArcsOnCircle

    /**
      * Test if this contains any positive-length arc segments.
      * @return True if and only if this arc collection is nonempty.
      */
    def nonEmpty: Boolean

    /**
      * Rotate about the given point in the counterclockwise direction.
      * @param rotationCenter Center of rotation.
      * @param angle Angle of rotation.
      * @return Image of this under the specified rotation.
      */
    def rotate(rotationCenter: Point, angle: Angle): ArcsOnCircle
  }

  // Shared traits for type members

  /**
    * Objects that have a well-defined center.
    */
  trait HasCenter {
    /**
      * The natural center of this object.
      * @return Center of this object.
      */
    def center: Point
  }

  /**
    * Objects that are associated with a single circle.
    */
  trait HasCircle extends HasCenter {
    /**
      * The circle associated with this object.
      * @return Circle of this object.
      */
    def circle: Circle

    /**
      * The natural center of this object, which is the center of its circle.
      * @return Center of this object.
      */
    override def center: Point = circle.center
  }
}
