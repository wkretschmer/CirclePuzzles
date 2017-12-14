package circlepuzzles.geometry

import circlepuzzles.math.FixedPoint

/**
  * Vectors in 3-dimensional Euclidean space. Vectors are represented by their (x, y, z) coordinates.
  * @param x X-coordinate of this vector.
  * @param y Y-coordinate of this vector.
  * @param z Z-coordinate of this vector.
  */
case class Vector3D(x: FixedPoint, y: FixedPoint, z: FixedPoint) {
  /**
    * Scalar multiplication.
    * @param scalar Scalar represented by a `FixedPoint`.
    * @return Scalar product of `this` with the given argument.
    */
  def *(scalar: FixedPoint): Vector3D = {
    // Multiply entrywise
    Vector3D(x * scalar, y * scalar, z * scalar)
  }

  /**
    * Vector addition.
    * @param that Vector to add.
    * @return Vector sum `this + that`.
    */
  def +(that: Vector3D): Vector3D = {
    // Add entrywise
    Vector3D(x + that.x, y + that.y, z + that.z)
  }

  /**
    * Vector subtraction.
    * @param that Vector to subtract.
    * @return Vector difference `this - that`. Equivalent to `this + that * -FixedPoint.One`.
    */
  def -(that: Vector3D): Vector3D = {
    // Subtract entrywise
    Vector3D(x - that.x, y - that.y, z - that.z)
  }

  /**
    * Vector dot product (i.e. Euclidean inner product).
    * @param that Vector to combine in dot product.
    * @return Dot product `this . that`.
    */
  def dotProduct(that: Vector3D): FixedPoint = {
    // Sum of products between corresponding entries
    x * that.x + y * that.y + z * that.z
  }

  /**
    * Squared norm of this vector, i.e. `this . this`.
    * @return Squared norm of this vector.
    */
  def normSquared: FixedPoint = {
    // |x|^2 = x . x
    dotProduct(this)
  }

  /**
    * Norm (i.e length) of this vector.
    * @return Norm of this vector as a nonnegative `FixedPoint`.
    */
  def norm: FixedPoint = {
    FixedPoint.sqrt(normSquared)
  }

  /**
    * Convex angle between this and another vector. Returns 0 if either vector is the 0 vector.
    * @param that Vector that forms an angle with `this`.
    * @return Angle between `this` and `that`. The returned angle is convex, i.e. in the range [0,pi].
    */
  def convexAngle(that: Vector3D): Angle = {
    // x . y = |x| * |y| * cos(theta)
    val cosAngle = dotProduct(that) / (norm * that.norm)
    new Angle(FixedPoint.acos(cosAngle))
  }

  /**
    * Vector cross product.
    * @param that Vector to compute cross product with.
    * @return Cross product `this x that`.
    */
  def crossProduct(that: Vector3D): Vector3D = {
    val newX = y * that.z - z * that.y
    val newY = z * that.x - x * that.z
    val newZ = x * that.y - y * that.x
    Vector3D(newX, newY, newZ)
  }
}
