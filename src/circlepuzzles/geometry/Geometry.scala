package circlepuzzles.geometry

/**
  * Objects that specify computations for a particular two-dimensional plane-like geometric setting. Subclasses of this
  * trait are generally singleton objects.
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
    * Type of arcs. An arc is defined as a set of points on a circle between a given start and end point, inclusive.
    */
  type Arc <: BaseArc

  /**
    * Type of disjoint arc segments on a single circle.
    */
  type ArcsOnCircle <: BaseArcsOnCircle

  /**
    * Type of disks in this geometry. A disk is defined as a closed set of points whose boundary is a circle. Depending
    * on context, this can either be the interior or exterior of a circle.
    */
  type Disk <: BaseDisk


  // Base traits for type members

  /**
    * Base trait for points. Immutable.
    */
  trait BasePoint { this: Point => // Every BasePoint must also be a Point

  }

  /**
    * Base trait for circles. Immutable.
    */
  trait BaseCircle { this: Circle => // Every BaseCircle must also be a Circle
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
  }

  /**
    * Base trait for disks. Immutable.
    */
  trait BaseDisk { this: Disk => // Every BaseDisk must also be a Disk
    /**
      * The boundary of this disk, which is a circle.
      * @return Circle corresponding to this disk's boundary.
      */
    def circle: Circle

    /**
      * The point which is equidistant from every point on the boundary.
      * @return The center of this disk.
      */
    def center: Point

    /**
      * Test if the given point is in the interior or on the boundary of this disk.
      * @param pt Point to test for membership.
      * @return True if and only if this disk contains the given point.
      */
    def contains(pt: Point): Boolean

    /**
      * Test if the given point is in the interior of this disk.
      * @param pt Point to test for membership.
      * @return True if and only if this disk strictly contains the given point.
      */
    def strictlyContains(pt: Point): Boolean
  }

  /**
    * Base trait for arcs. Immutable.
    */
  trait BaseArc { this: Arc => // Every BaseArc must also be an Arc
    /**
      * Attempts to combine two arcs around the same circle. If `this` and `that` belong to the same circle and share at
      * least one point (possibly an endpoint), this returns `Some(union)`, where `union` is the arc formed by the union
      * of the arcs. Otherwise, if the arcs are disjoint or belong to different circles, returns `None`.
      * @param that Arc to combine.
      * @return Union of `this` and `that`, or `None` if they can't be combined.
      */
    def union(that: Arc): Option[Arc]

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
      * Rotate this arc about the given point by the given angle, in the counterclockwise direction.
      * @param center Center of rotation.
      * @param angle Angle of rotation.
      * @return Rotation of this arc about the given point by the given angle.
      */
    def rotate(center: Point, angle: Angle): Arc
  }

  /**
    * Base trait for arcs on a circle. Immutable.
    */
  trait BaseArcsOnCircle { this: ArcsOnCircle => // Every BaseArcsOnCircle must also be an ArcsOnCircle
    /**
      * The circle to which arcs in this collection belong.
      * @return Circle corresponding to these arcs.
      */
    def circle: Circle

    /**
      * Compute the union of this with the given arcs by joining overlapping segments.
      * @param that Arcs around a the same circle (i.e. requires `this.circle == that.circle`).
      * @return `ArcsOnCircle` containing all arcs that belong to either `this` or `that`.
      * @throws IllegalArgumentException If `this.circle != that.circle`.
      */
    def sameCircleUnion(that: ArcsOnCircle): ArcsOnCircle

    /**
      * Compute the difference of this with the given arcs by subtracting overlapping segments.
      * @param that Arcs around a the same circle (i.e. requires `this.circle == that.circle`).
      * @return `ArcsOnCircle` containing all arcs that belong to `this` but not `that`.
      * @throws IllegalArgumentException If `this.circle != that.circle`.
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
      * Rotate these arcs about the given point by the given angle, in the counterclockwise direction.
      * @param center Center of rotation.
      * @param angle Angle of rotation.
      * @return Rotation of these arcs about the given point by the given angle.
      */
    def rotate(center: Point, angle: Angle): ArcsOnCircle
  }
}
