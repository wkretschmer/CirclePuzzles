package circlepuzzles.geometry

/**
  * Geometry of the Euclidean plane.
  */
object PlanarGeometry extends Geometry {
  override type Point = planar.Point
  override type Circle = planar.Circle
  override type Arc = planar.Arc
  override type Disk = planar.Disk
  override type ArcsOnCircle = planar.ArcsOnCircle
}
