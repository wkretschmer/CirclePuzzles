package circlepuzzles.geometry

/**
  * Classes that use a single geometry type for computations. Often, getting the particular types works by calling
  * `import geom._` at the top of a subclass.
  * @param geom The particular geometry to use for computations.
  */
abstract class HasGeometry(val geom: Geometry)
