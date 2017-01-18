package circlepuzzles.geometry

object SphericalGeometry extends Geometry {
  override type Point = spherical.Point
  override type Circle = spherical.Circle
  override type Disk = spherical.Disk
  override type Arc = spherical.Arc
  override type ArcsOnCircle = spherical.ArcsOnCircle
}
