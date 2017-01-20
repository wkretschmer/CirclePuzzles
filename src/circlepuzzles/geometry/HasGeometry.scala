package circlepuzzles.geometry

/**
  * Classes that use a single geometry type for computations. Often, getting the particular types works by calling
  * `import geom._` at the top of a subclass.
  */
abstract class HasGeometry {
  /**
    * The particular geometry to use for computations.
    */
  val geom: Geometry
}

/**
  * Classes that use Euclidean planar geometry.
  */
trait HasPlanarGeometry extends HasGeometry {
  override val geom = PlanarGeometry
}

/**
  * Classes that use geometry of the unit sphere.
  */
trait HasSphericalGeometry extends HasGeometry {
  override val geom = SphericalGeometry
}
